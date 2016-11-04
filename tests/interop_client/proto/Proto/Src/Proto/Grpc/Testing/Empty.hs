{- This file was auto-generated from src/proto/grpc/testing/empty.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Src.Proto.Grpc.Testing.Empty where
import qualified Prelude
import qualified Data.Int
import qualified Data.Word

import qualified Data.ProtoLens
import qualified Data.ProtoLens.Message.Enum
import qualified Lens.Family2
import qualified Lens.Family2.Unchecked
import qualified Data.Default.Class
import qualified Data.Text
import qualified Data.Map
import qualified Data.ByteString

data Empty = Empty{}
           deriving (Prelude.Show, Prelude.Eq)

instance Data.Default.Class.Default Empty where
        def = Empty{}

instance Data.ProtoLens.Message Empty where
        descriptor
          = let in
              Data.ProtoLens.MessageDescriptor (Data.Map.fromList [])
                (Data.Map.fromList [])