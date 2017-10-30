-- Copyright 2017 gRPC authors.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
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

#include <grpc/grpc.h>
#include "hs_grpc.h"

import qualified Foreign.Ptr as C
import           Foreign.Ptr (Ptr)
import qualified Foreign.ForeignPtr as C
import qualified Foreign.Marshal.Alloc as C
import qualified Foreign.Marshal.Array as C
import qualified Foreign.Storable as C

import Control.Monad

import Data.ByteString (ByteString)
{#import Network.Grpc.Lib.ByteBuffer#} (Slice, CSlice, toByteString, grpcSliceFromCopiedBuffer)

{#context lib = "grpc" prefix = "grpc" #}

{#pointer *grpc_metadata as MetadataPtr -> Metadata#}

data Metadata = Metadata !ByteString !ByteString !{#type uint32_t#} deriving (Show)

instance Eq Metadata where
  (Metadata a b _) == (Metadata x y _) = (a,b) == (x,y)

-- | Cast the pointer due to c2hs not resolving the type properly,
-- or not supporting our use case.
wonkyC2hsCast :: C.Ptr a -> C.Ptr b
wonkyC2hsCast = C.castPtr

metadataKey :: Ptr Metadata -> IO Slice
metadataKey p = do
  C.newForeignPtr_ (wonkyC2hsCast (p `C.plusPtr` {#offsetof grpc_metadata.key#}))

metadataValue :: Ptr Metadata -> IO Slice
metadataValue p = do
  C.newForeignPtr_ (wonkyC2hsCast (p `C.plusPtr` {#offsetof grpc_metadata.value#}))

instance C.Storable Metadata where
  sizeOf _ = {#sizeof grpc_metadata#}
  alignment _ = {#alignof grpc_metadata#}
  peek p = do
    keySlice <- metadataKey p
    key <- toByteString keySlice
    valueSlice <- metadataValue p
    value <- toByteString valueSlice
    flags <- {#get grpc_metadata.flags#} p
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
  forM_ (zip [0..] mds) $ \(i, md) ->
    writeMetadata md (arr `C.plusPtr` (i * {#sizeof grpc_metadata#}))
  return (C.castPtr arr, C.free arr)
  where
    writeMetadata (Metadata key value flags) arr_ptr = do
      keyPtr   <- C.newForeignPtr_ (arr_ptr `C.plusPtr` {#offsetof grpc_metadata.key#})
      valuePtr <- C.newForeignPtr_ (arr_ptr `C.plusPtr` {#offsetof grpc_metadata.value#})
      grpcSliceFromCopiedBuffer key   keyPtr
      grpcSliceFromCopiedBuffer value valuePtr
      {#set grpc_metadata.flags#} arr_ptr flags

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
  { %`Slice' } -> `Bool' #}

-- | Validate a non-binary value.
{#fun pure grpc_header_nonbin_value_is_legal as isNonBinValueValid
  { %`Slice' } -> `Bool' #}

-- | Is the key a binary key?
{#fun pure grpc_is_binary_header as isBinaryHeader
  { %`Slice' } -> `Bool' #}