-- Copyright (c) 2016, Google Inc.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of Google Inc. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL Google Inc. BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface, MagicHash, TypeSynonymInstances, FlexibleInstances #-}
module Network.Grpc.Lib.ByteBuffer
  ( CByteBuffer
  , ByteBuffer

  , Slice
  , CSlice
  , sliceFromCopy
  , toByteString
  , grpcSliceRef
  , grpcSliceUnref
  , sliceFromStaticByteString
  , grpcSliceFromCopiedBuffer
  , mallocSlice

  , fromByteString
  , toLazyByteString
  , byteBufferLength
  , mallocCByteBuffer
  , freeCByteBuffer
  , byteBufferDestroy

  , CByteBufferReader
  ) where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc as C
import Foreign.Ptr

import Control.Exception (finally)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

import Data.String

import GHC.Prim (Addr#)
import GHC.Ptr (Ptr(..))

import System.IO.Unsafe ( unsafePerformIO )

#include <grpc/grpc.h>
#include <grpc/byte_buffer.h>
#include <grpc/byte_buffer_reader.h>
#include "hs_byte_buffer.h"
#include "hs_grpc.h"

{#context lib = "grpc" prefix = "grpc" #}

data CByteBuffer
{#pointer *byte_buffer as ByteBuffer -> CByteBuffer#}

mallocCByteBuffer :: IO (Ptr CByteBuffer)
mallocCByteBuffer =
  C.mallocBytes {#sizeof grpc_byte_buffer#}

freeCByteBuffer :: Ptr CByteBuffer -> IO ()
freeCByteBuffer =
  C.free

{#fun unsafe byte_buffer_destroy as ^
  {`ByteBuffer'} -> `()' #}

data CByteBufferReader
{#pointer *byte_buffer_reader as ByteBufferReader -> CByteBufferReader#}

data CSlice
{#pointer *grpc_slice as Slice foreign -> CSlice #}

type SizeT = {#type size_t#}

fromByteString :: ByteString -> IO ByteBuffer
fromByteString = hsRawByteBufferCreate

withByteString :: ByteString -> ((Ptr CChar, CULong) -> IO a) -> IO a
withByteString bs act = do
  let (fPtr, offset, len) = B.toForeignPtr bs
  withForeignPtr fPtr $ \ptr -> act (ptr `plusPtr` offset, fromIntegral len)

{#fun unsafe hs_raw_byte_buffer_create as ^
  {withByteString* `ByteString'&} -> `ByteBuffer' #}

toByteString :: Slice -> IO ByteString
toByteString slice0 = withForeignPtr slice0 $ \slice -> do
  refcount <- {#get grpc_slice->refcount#} slice
  if refcount == nullPtr
    then fromInlined slice
    else fromRefcounted slice
  where
    fromInlined slice = do
      len <- {#get grpc_slice->data.inlined.length#} slice
      ptr <- {#get grpc_slice->data.inlined.bytes#} slice
      B.packCStringLen (castPtr ptr, fromIntegral len)
    fromRefcounted slice = do
      len <- {#get grpc_slice->data.refcounted.length#} slice
      ptr <- {#get grpc_slice->data.refcounted.bytes#} slice
      B.packCStringLen (castPtr ptr, fromIntegral len)

mallocSlice :: IO Slice
mallocSlice =
  mallocForeignPtrBytes {#sizeof grpc_slice#}

sliceFromCopy :: ByteString -> IO Slice
sliceFromCopy bs = do
  slice <- mallocSlice
  grpcSliceFromCopiedBuffer bs slice
  addForeignPtrFinalizer grpcSlideUnrefFinalizer slice
  return slice

instance IsString Slice where
  fromString = unsafePerformIO . sliceFromCopy . B8.pack
  {-# NOINLINE fromString #-}

{#fun unsafe hs_grpc_slice_from_copied_buffer as grpcSliceFromCopiedBuffer
  { withByteString* `ByteString'&
  , `Slice' } -> `()' #}

-- | Make a Slice without a reference counter.
sliceFromStaticByteString :: ByteString -> IO Slice
sliceFromStaticByteString bs = do
  slice <- mallocSlice
  -- Since the underlying string is static, there is no need to
  -- call grpc_slice_unref. All we need to do is to free the memory for
  -- the grpc_slice itself.
  withByteString bs $ \(ptr, _) -> grpcSliceFromStaticString ptr slice
  return slice

-- | Make a Slice without a reference counter.
sliceFromStaticString# :: Addr# -> IO Slice
sliceFromStaticString# addr# = do
  slice <- mallocSlice
  grpcSliceFromStaticString (Ptr addr#) slice
  return slice

{#fun unsafe hs_grpc_slice_from_static_string as grpcSliceFromStaticString
  { id `Ptr CChar'
  , `Slice' } -> `()' #}

toLazyByteString :: ByteBuffer -> IO L.ByteString
toLazyByteString bb =
  allocaByteBufferReader $ \ bbr -> do
  slice <- mallocSlice
  ok <- byteBufferReaderInit bbr bb
  if ok
    then finally (go bbr slice []) (byteBufferReaderDestroy bbr)
    else return L.empty -- TODO: assert
  where
    go bbr slice acc = do
      ok <- byteBufferReaderNext bbr slice
      if ok
        then do
          bs <- toByteString slice
          grpcSliceUnref slice
          go bbr slice (bs:acc)
        else return $! L.fromChunks (reverse acc)

allocaByteBufferReader :: (ByteBufferReader -> IO a) -> IO a
allocaByteBufferReader act = do
  allocaBytes {#sizeof grpc_byte_buffer_reader#} $ \p -> act p

-- | Initialize a 'ByteBufferReader' for the given 'ByteBuffer'.
-- If return True, the initialization was successful and the caller is
-- responsible for calling 'byteBufferReaderDestroy'.
{#fun unsafe byte_buffer_reader_init as ^
  {`ByteBufferReader', `ByteBuffer'} -> `Bool' #}

-- | Updates the 'Slice' with the next piece of data from the reader
-- and returns True. Returns False at the end of the stream. The caller
-- is responsible for calling 'grpc_slice_unref' on the result.
{#fun unsafe byte_buffer_reader_next as ^
  {`ByteBufferReader', `Slice'} -> `Bool'#}

-- | Clean up a 'ByteBufferReader'.
{#fun unsafe byte_buffer_reader_destroy as ^
  {`ByteBufferReader'} -> `()'#}

-- | /O(1)/. Return the length of a 'ByteBuffer'.
{#fun unsafe byte_buffer_length as ^
  {`ByteBuffer'} -> `SizeT' id #}

-- | Updates the 'Slice' with a slice of all the data merged.
{#fun unsafe hs_grpc_byte_buffer_reader_readall as ^
  {`ByteBufferReader', `Slice'} -> `()' #}

-- | Ref a 'Slice'. When the reference counter reaches zero, the slice will
-- be deallocated.
{#fun unsafe grpc_slice_ref as ^
  {%`Slice'} -> `()' #}

-- | Unref a 'Slice'. When the reference counter reaches zero, the slice will
-- be deallocated.
{#fun unsafe grpc_slice_unref as ^
  {%`Slice'} -> `()' #}

foreign import ccall "hs_byte_buffer.h &hs_grpc_slice_unref"
  grpcSlideUnrefFinalizer :: FunPtr (Ptr CSlice -> IO ())
