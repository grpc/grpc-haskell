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
{-# LANGUAGE StandaloneDeriving, ForeignFunctionInterface, OverloadedStrings #-}
module Network.Grpc.Lib.ByteBuffer
  ( CByteBuffer
  , fromByteString
  , addBBFinalizer
  , toLazyByteString

  , CByteBufferReader
  ) where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Control.Exception (bracket)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L

#include <grpc/byte_buffer.h>
#include <grpc/byte_buffer_reader.h>
#include "hs_byte_buffer.h"

{#context lib = "grpc" prefix = "grpc" #}

data CByteBuffer
{#pointer *byte_buffer as ByteBuffer foreign -> CByteBuffer#}
data CByteBufferReader
{#pointer *byte_buffer_reader as ByteBufferReader -> CByteBufferReader#}

data CSlice
{#pointer *gpr_slice as Slice -> CSlice#}

fromByteString :: ByteString -> IO ByteBuffer
fromByteString = hsRawByteBufferCreate

withByteString :: ByteString -> ((Ptr CChar, CULong) -> IO a) -> IO a
withByteString bs act = do
  let (fPtr, offset, len) = B.toForeignPtr bs
  withForeignPtr fPtr $ \ptr -> act (ptr `plusPtr` offset, fromIntegral len)

{#fun unsafe hs_raw_byte_buffer_create as ^
  {withByteString* `ByteString'&} -> `ByteBuffer' addBBFinalizer* #}

addBBFinalizer :: Ptr CByteBuffer -> IO ByteBuffer
addBBFinalizer bb = newForeignPtr grpc_byte_buffer_destroy bb

foreign import ccall "Network/Grpc/Core/ByteBuffer.chs.h &grpc_byte_buffer_destroy"
  grpc_byte_buffer_destroy :: FinalizerPtr CByteBuffer

toByteString :: Slice -> IO ByteString
toByteString slice = do
  refcount <- {#get gpr_slice->refcount#} slice
  if refcount == nullPtr
    then fromInlined
    else fromRefcounted
  where
    fromInlined = do
      len <- {#get gpr_slice->data.inlined.length#} slice
      ptr <- {#get gpr_slice->data.inlined.bytes#} slice
      B.packCStringLen (castPtr ptr, fromIntegral len)
    fromRefcounted = do
      len <- {#get gpr_slice->data.refcounted.length#} slice
      ptr <- {#get gpr_slice->data.refcounted.bytes#} slice
      B.packCStringLen (castPtr ptr, fromIntegral len)

toLazyByteString :: ByteBuffer -> IO L.ByteString
toLazyByteString bb =
  bracket
    (byteBufferReaderInit bb)
    (byteBufferReaderDestroy)
    (\bbr -> L.fromChunks <$> go bbr [])
  where
    go bbr acc = do
      (tag, slice) <- byteBufferReaderNext bbr
      case tag of
        0 -> return $ reverse acc
        _ -> do
          bs <- toByteString slice
          go bbr (bs:acc)

{#fun unsafe byte_buffer_reader_init as ^
  {allocaByteBufferReader- `ByteBufferReader' id, `ByteBuffer'} -> `()' #}

allocaSlice :: (Slice -> IO a) -> IO a
allocaSlice act = do
  allocaBytes {#sizeof gpr_slice#} $ \p -> act p

allocaByteBufferReader :: (ByteBufferReader -> IO a) -> IO a
allocaByteBufferReader act = do
  allocaBytes {#sizeof grpc_byte_buffer_reader#} $ \p -> act p

{#fun unsafe byte_buffer_reader_next as ^
  {`ByteBufferReader', allocaSlice- `Slice' id} -> `Int' fromIntegral#}

{#fun unsafe byte_buffer_reader_destroy as ^
  {`ByteBufferReader'} -> `()'#}
