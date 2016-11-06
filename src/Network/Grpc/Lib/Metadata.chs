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
module Network.Grpc.Lib.Metadata (
    Metadata(..)
  , MetadataPtr
  , mallocMetadata

  , isKeyValid
  , isNonBinValueValid
  , isBinaryHeader

  , MetadataArray
  , mallocMetadataArray
  , readMetadataArray
  , freeMetadataArray
  ) where

import qualified Foreign.C.Types as C
import qualified Foreign.Ptr as C
import qualified Foreign.ForeignPtr as C
import qualified Foreign.Marshal.Alloc as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.Storable as C

import Control.Monad
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString (ByteString)

#include <grpc/grpc.h>
#include "hs_grpc.h"

{#context lib = "grpc" prefix = "grpc" #}

{#pointer *grpc_metadata as MetadataPtr -> Metadata#}

data Metadata = Metadata !ByteString !ByteString !{#type uint32_t#} deriving (Show)

instance Eq Metadata where
  (Metadata a b _) == (Metadata x y _) = (a,b) == (x,y)

instance C.Storable Metadata where
  sizeOf _ = {#sizeof grpc_metadata#}
  alignment _ = {#alignof grpc_metadata#}
  peek p = do
    keyPtr <- {#get grpc_metadata->key#} p
    valuePtr <- {#get grpc_metadata->value#} p
    valueLength <- {#get grpc_metadata->value_length#} p
    key <- B.packCString keyPtr
    value <- B.packCStringLen (valuePtr, fromIntegral valueLength)
    flags <- {#get grpc_metadata->flags#} p
    return $! Metadata key value flags
  poke _ _ = error "Storable Metadata: poke not implemented"

data CMetadataArray
{#pointer *metadata_array as MetadataArray -> CMetadataArray#}

{#fun unsafe metadata_array_init as ^
  {`MetadataArray'} -> `()'#}

{#fun unsafe metadata_array_destroy as ^
  {`MetadataArray'} -> `()'#}

mallocMetadata :: [Metadata] -> IO (MetadataPtr, IO ())
mallocMetadata mds = do
  arr <- C.mallocBytes (length mds * {#sizeof grpc_metadata#})
  ptrs <- forM (zip [0..] mds) $ \(i, md) ->
    writeMetadata md (arr `C.plusPtr` (i * {#sizeof grpc_metadata#}))
  return (C.castPtr arr, mapM_ C.free (arr : concat ptrs))
  where
    writeMetadata (Metadata key value flags) arr_ptr = do
      key_ptr <- asCString key
      value_ptr <- asCString value
      {#set grpc_metadata->key#} arr_ptr key_ptr
      {#set grpc_metadata->value#} arr_ptr value_ptr
      {#set grpc_metadata->value_length#} arr_ptr (fromIntegral (B.length value))
      {#set grpc_metadata->flags#} arr_ptr flags
      return [key_ptr, value_ptr]

    -- | Create null terminated C-string.
    asCString :: ByteString -> IO (C.Ptr C.CChar)
    asCString (B.PS fp o l) = do
      buf <- C.mallocBytes (l+1)
      C.withForeignPtr fp $ \p -> do
        B.memcpy buf (p `C.plusPtr` o) (fromIntegral l)
        C.pokeByteOff buf l (0::Word8)
      return (C.castPtr buf)

mallocMetadataArray :: IO (C.Ptr CMetadataArray)
mallocMetadataArray = do 
  ptr <- C.mallocBytes {#sizeof grpc_metadata_array#}
  metadataArrayInit ptr
  return ptr

freeMetadataArray :: MetadataArray -> IO ()
freeMetadataArray arr = do
  metadataArrayDestroy arr
  C.free arr

readMetadataArray :: MetadataArray -> IO [Metadata]
readMetadataArray arr = do
  count <- {#get grpc_metadata_array->count#} arr
  metadataPtr <- {#get grpc_metadata_array->metadata#} arr
  C.peekArray (fromIntegral count) metadataPtr

-- | Validate the key of a metadata pair.
{#fun pure grpc_header_key_is_legal as isKeyValid
  { 'useAsCStringLen'* `B.ByteString'&} -> `Bool' #}

-- | Validate a non-binary value.
{#fun pure grpc_header_nonbin_value_is_legal as isNonBinValueValid
  { 'useAsCStringLen'* `B.ByteString'&} -> `Bool' #}

-- | Is the key a binary key?
{#fun pure grpc_is_binary_header as isBinaryHeader
  { 'useAsCStringLen'* `B.ByteString'&} -> `Bool' #}

useAsCStringLen :: Num a => B.ByteString -> ((C.Ptr C.CChar, a) -> IO b) -> IO b
useAsCStringLen bs act =
  B.useAsCStringLen bs $ \(ptr, len) -> act (ptr, fromIntegral len)
