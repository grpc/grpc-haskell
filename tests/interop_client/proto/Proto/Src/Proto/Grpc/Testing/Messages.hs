{- This file was auto-generated from src/proto/grpc/testing/messages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Src.Proto.Grpc.Testing.Messages where
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

data BoolValue = BoolValue{_BoolValue'value :: Prelude.Bool}
               deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "value" BoolValue = Prelude.Bool

instance Data.ProtoLens.HasField "value" BoolValue BoolValue where
        field _
          = Lens.Family2.Unchecked.lens _BoolValue'value
              (\ x__ y__ -> x__{_BoolValue'value = y__})

instance Data.Default.Class.Default BoolValue where
        def = BoolValue{_BoolValue'value = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message BoolValue where
        descriptor
          = let value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional value)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, value__field_descriptor)])
                (Data.Map.fromList [("value", value__field_descriptor)])

data EchoStatus = EchoStatus{_EchoStatus'code :: Data.Int.Int32,
                             _EchoStatus'message :: Data.Text.Text}
                deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "code" EchoStatus =
     Data.Int.Int32

instance Data.ProtoLens.HasField "code" EchoStatus EchoStatus where
        field _
          = Lens.Family2.Unchecked.lens _EchoStatus'code
              (\ x__ y__ -> x__{_EchoStatus'code = y__})

type instance Data.ProtoLens.Field "message" EchoStatus =
     Data.Text.Text

instance Data.ProtoLens.HasField "message" EchoStatus EchoStatus
         where
        field _
          = Lens.Family2.Unchecked.lens _EchoStatus'message
              (\ x__ y__ -> x__{_EchoStatus'message = y__})

instance Data.Default.Class.Default EchoStatus where
        def
          = EchoStatus{_EchoStatus'code = Data.ProtoLens.fieldDefault,
                       _EchoStatus'message = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message EchoStatus where
        descriptor
          = let code__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "code"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional code)
                message__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "message"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional message)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, code__field_descriptor),
                    (Data.ProtoLens.Tag 2, message__field_descriptor)])
                (Data.Map.fromList
                   [("code", code__field_descriptor),
                    ("message", message__field_descriptor)])

data Payload = Payload{_Payload'type' :: PayloadType,
                       _Payload'body :: Data.ByteString.ByteString}
             deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "type'" Payload = PayloadType

instance Data.ProtoLens.HasField "type'" Payload Payload where
        field _
          = Lens.Family2.Unchecked.lens _Payload'type'
              (\ x__ y__ -> x__{_Payload'type' = y__})

type instance Data.ProtoLens.Field "body" Payload =
     Data.ByteString.ByteString

instance Data.ProtoLens.HasField "body" Payload Payload where
        field _
          = Lens.Family2.Unchecked.lens _Payload'body
              (\ x__ y__ -> x__{_Payload'body = y__})

instance Data.Default.Class.Default Payload where
        def
          = Payload{_Payload'type' = Data.Default.Class.def,
                    _Payload'body = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message Payload where
        descriptor
          = let type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor PayloadType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional type')
                body__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "body"
                      (Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional body)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, type'__field_descriptor),
                    (Data.ProtoLens.Tag 2, body__field_descriptor)])
                (Data.Map.fromList
                   [("type", type'__field_descriptor),
                    ("body", body__field_descriptor)])

data PayloadType = COMPRESSABLE
                 deriving (Prelude.Show, Prelude.Eq)

instance Data.Default.Class.Default PayloadType where
        def = COMPRESSABLE

instance Data.ProtoLens.FieldDefault PayloadType where
        fieldDefault = COMPRESSABLE

instance Data.ProtoLens.MessageEnum PayloadType where
        maybeToEnum 0 = Prelude.Just COMPRESSABLE
        maybeToEnum _ = Prelude.Nothing
        showEnum COMPRESSABLE = "COMPRESSABLE"
        readEnum "COMPRESSABLE" = Prelude.Just COMPRESSABLE
        readEnum _ = Prelude.Nothing

instance Prelude.Enum PayloadType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum PayloadType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum COMPRESSABLE = 0
        succ COMPRESSABLE
          = Prelude.error
              "Ident \"PayloadType\".Ident \"succ\": bad argument Ident \"COMPRESSABLE\". This value would be out of bounds."
        pred COMPRESSABLE
          = Prelude.error
              "Ident \"PayloadType\".Ident \"pred\": bad argument Ident \"COMPRESSABLE\". This value would be out of bounds."
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo

instance Prelude.Bounded PayloadType where
        minBound = COMPRESSABLE
        maxBound = COMPRESSABLE

data ReconnectInfo = ReconnectInfo{_ReconnectInfo'passed ::
                                   Prelude.Bool,
                                   _ReconnectInfo'backoffMs :: [Data.Int.Int32]}
                   deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "passed" ReconnectInfo =
     Prelude.Bool

instance Data.ProtoLens.HasField "passed" ReconnectInfo
         ReconnectInfo where
        field _
          = Lens.Family2.Unchecked.lens _ReconnectInfo'passed
              (\ x__ y__ -> x__{_ReconnectInfo'passed = y__})

type instance Data.ProtoLens.Field "backoffMs" ReconnectInfo =
     [Data.Int.Int32]

instance Data.ProtoLens.HasField "backoffMs" ReconnectInfo
         ReconnectInfo where
        field _
          = Lens.Family2.Unchecked.lens _ReconnectInfo'backoffMs
              (\ x__ y__ -> x__{_ReconnectInfo'backoffMs = y__})

instance Data.Default.Class.Default ReconnectInfo where
        def
          = ReconnectInfo{_ReconnectInfo'passed =
                            Data.ProtoLens.fieldDefault,
                          _ReconnectInfo'backoffMs = []}

instance Data.ProtoLens.Message ReconnectInfo where
        descriptor
          = let passed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "passed"
                      (Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional passed)
                backoffMs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "backoff_ms"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked backoffMs)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, passed__field_descriptor),
                    (Data.ProtoLens.Tag 2, backoffMs__field_descriptor)])
                (Data.Map.fromList
                   [("passed", passed__field_descriptor),
                    ("backoff_ms", backoffMs__field_descriptor)])

data ReconnectParams = ReconnectParams{_ReconnectParams'maxReconnectBackoffMs
                                       :: Data.Int.Int32}
                     deriving (Prelude.Show, Prelude.Eq)

type instance
     Data.ProtoLens.Field "maxReconnectBackoffMs" ReconnectParams =
     Data.Int.Int32

instance Data.ProtoLens.HasField "maxReconnectBackoffMs"
         ReconnectParams ReconnectParams where
        field _
          = Lens.Family2.Unchecked.lens
              _ReconnectParams'maxReconnectBackoffMs
              (\ x__ y__ -> x__{_ReconnectParams'maxReconnectBackoffMs = y__})

instance Data.Default.Class.Default ReconnectParams where
        def
          = ReconnectParams{_ReconnectParams'maxReconnectBackoffMs =
                              Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message ReconnectParams where
        descriptor
          = let maxReconnectBackoffMs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "max_reconnect_backoff_ms"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         maxReconnectBackoffMs)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, maxReconnectBackoffMs__field_descriptor)])
                (Data.Map.fromList
                   [("max_reconnect_backoff_ms",
                     maxReconnectBackoffMs__field_descriptor)])

data ResponseParameters = ResponseParameters{_ResponseParameters'size
                                             :: Data.Int.Int32,
                                             _ResponseParameters'intervalUs :: Data.Int.Int32,
                                             _ResponseParameters'compressed ::
                                             Prelude.Maybe BoolValue}
                        deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "size" ResponseParameters =
     Data.Int.Int32

instance Data.ProtoLens.HasField "size" ResponseParameters
         ResponseParameters where
        field _
          = Lens.Family2.Unchecked.lens _ResponseParameters'size
              (\ x__ y__ -> x__{_ResponseParameters'size = y__})

type instance Data.ProtoLens.Field "intervalUs" ResponseParameters
     = Data.Int.Int32

instance Data.ProtoLens.HasField "intervalUs" ResponseParameters
         ResponseParameters where
        field _
          = Lens.Family2.Unchecked.lens _ResponseParameters'intervalUs
              (\ x__ y__ -> x__{_ResponseParameters'intervalUs = y__})

type instance Data.ProtoLens.Field "compressed" ResponseParameters
     = BoolValue

instance Data.ProtoLens.HasField "compressed" ResponseParameters
         ResponseParameters where
        field _
          = (Prelude..) maybe'compressed
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'compressed" ResponseParameters =
     Prelude.Maybe BoolValue

instance Data.ProtoLens.HasField "maybe'compressed"
         ResponseParameters ResponseParameters where
        field _
          = Lens.Family2.Unchecked.lens _ResponseParameters'compressed
              (\ x__ y__ -> x__{_ResponseParameters'compressed = y__})

instance Data.Default.Class.Default ResponseParameters where
        def
          = ResponseParameters{_ResponseParameters'size =
                                 Data.ProtoLens.fieldDefault,
                               _ResponseParameters'intervalUs = Data.ProtoLens.fieldDefault,
                               _ResponseParameters'compressed = Prelude.Nothing}

instance Data.ProtoLens.Message ResponseParameters where
        descriptor
          = let size__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "size"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional size)
                intervalUs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "interval_us"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional intervalUs)
                compressed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "compressed"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor BoolValue)
                      (Data.ProtoLens.OptionalField maybe'compressed)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, size__field_descriptor),
                    (Data.ProtoLens.Tag 2, intervalUs__field_descriptor),
                    (Data.ProtoLens.Tag 3, compressed__field_descriptor)])
                (Data.Map.fromList
                   [("size", size__field_descriptor),
                    ("interval_us", intervalUs__field_descriptor),
                    ("compressed", compressed__field_descriptor)])

data SimpleRequest = SimpleRequest{_SimpleRequest'responseType ::
                                   PayloadType,
                                   _SimpleRequest'responseSize :: Data.Int.Int32,
                                   _SimpleRequest'payload :: Prelude.Maybe Payload,
                                   _SimpleRequest'fillUsername :: Prelude.Bool,
                                   _SimpleRequest'fillOauthScope :: Prelude.Bool,
                                   _SimpleRequest'responseCompressed :: Prelude.Maybe BoolValue,
                                   _SimpleRequest'responseStatus :: Prelude.Maybe EchoStatus,
                                   _SimpleRequest'expectCompressed :: Prelude.Maybe BoolValue}
                   deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "responseType" SimpleRequest =
     PayloadType

instance Data.ProtoLens.HasField "responseType" SimpleRequest
         SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'responseType
              (\ x__ y__ -> x__{_SimpleRequest'responseType = y__})

type instance Data.ProtoLens.Field "responseSize" SimpleRequest =
     Data.Int.Int32

instance Data.ProtoLens.HasField "responseSize" SimpleRequest
         SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'responseSize
              (\ x__ y__ -> x__{_SimpleRequest'responseSize = y__})

type instance Data.ProtoLens.Field "payload" SimpleRequest =
     Payload

instance Data.ProtoLens.HasField "payload" SimpleRequest
         SimpleRequest where
        field _
          = (Prelude..) maybe'payload
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance Data.ProtoLens.Field "maybe'payload" SimpleRequest =
     Prelude.Maybe Payload

instance Data.ProtoLens.HasField "maybe'payload" SimpleRequest
         SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'payload
              (\ x__ y__ -> x__{_SimpleRequest'payload = y__})

type instance Data.ProtoLens.Field "fillUsername" SimpleRequest =
     Prelude.Bool

instance Data.ProtoLens.HasField "fillUsername" SimpleRequest
         SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'fillUsername
              (\ x__ y__ -> x__{_SimpleRequest'fillUsername = y__})

type instance Data.ProtoLens.Field "fillOauthScope" SimpleRequest =
     Prelude.Bool

instance Data.ProtoLens.HasField "fillOauthScope" SimpleRequest
         SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'fillOauthScope
              (\ x__ y__ -> x__{_SimpleRequest'fillOauthScope = y__})

type instance
     Data.ProtoLens.Field "responseCompressed" SimpleRequest = BoolValue

instance Data.ProtoLens.HasField "responseCompressed" SimpleRequest
         SimpleRequest where
        field _
          = (Prelude..) maybe'responseCompressed
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'responseCompressed" SimpleRequest =
     Prelude.Maybe BoolValue

instance Data.ProtoLens.HasField "maybe'responseCompressed"
         SimpleRequest SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'responseCompressed
              (\ x__ y__ -> x__{_SimpleRequest'responseCompressed = y__})

type instance Data.ProtoLens.Field "responseStatus" SimpleRequest =
     EchoStatus

instance Data.ProtoLens.HasField "responseStatus" SimpleRequest
         SimpleRequest where
        field _
          = (Prelude..) maybe'responseStatus
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'responseStatus" SimpleRequest =
     Prelude.Maybe EchoStatus

instance Data.ProtoLens.HasField "maybe'responseStatus"
         SimpleRequest SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'responseStatus
              (\ x__ y__ -> x__{_SimpleRequest'responseStatus = y__})

type instance Data.ProtoLens.Field "expectCompressed" SimpleRequest
     = BoolValue

instance Data.ProtoLens.HasField "expectCompressed" SimpleRequest
         SimpleRequest where
        field _
          = (Prelude..) maybe'expectCompressed
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'expectCompressed" SimpleRequest =
     Prelude.Maybe BoolValue

instance Data.ProtoLens.HasField "maybe'expectCompressed"
         SimpleRequest SimpleRequest where
        field _
          = Lens.Family2.Unchecked.lens _SimpleRequest'expectCompressed
              (\ x__ y__ -> x__{_SimpleRequest'expectCompressed = y__})

instance Data.Default.Class.Default SimpleRequest where
        def
          = SimpleRequest{_SimpleRequest'responseType =
                            Data.Default.Class.def,
                          _SimpleRequest'responseSize = Data.ProtoLens.fieldDefault,
                          _SimpleRequest'payload = Prelude.Nothing,
                          _SimpleRequest'fillUsername = Data.ProtoLens.fieldDefault,
                          _SimpleRequest'fillOauthScope = Data.ProtoLens.fieldDefault,
                          _SimpleRequest'responseCompressed = Prelude.Nothing,
                          _SimpleRequest'responseStatus = Prelude.Nothing,
                          _SimpleRequest'expectCompressed = Prelude.Nothing}

instance Data.ProtoLens.Message SimpleRequest where
        descriptor
          = let responseType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_type"
                      (Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor PayloadType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional responseType)
                responseSize__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_size"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional responseSize)
                payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Payload)
                      (Data.ProtoLens.OptionalField maybe'payload)
                fillUsername__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "fill_username"
                      (Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional fillUsername)
                fillOauthScope__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "fill_oauth_scope"
                      (Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional fillOauthScope)
                responseCompressed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_compressed"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor BoolValue)
                      (Data.ProtoLens.OptionalField maybe'responseCompressed)
                responseStatus__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_status"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor EchoStatus)
                      (Data.ProtoLens.OptionalField maybe'responseStatus)
                expectCompressed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "expect_compressed"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor BoolValue)
                      (Data.ProtoLens.OptionalField maybe'expectCompressed)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, responseType__field_descriptor),
                    (Data.ProtoLens.Tag 2, responseSize__field_descriptor),
                    (Data.ProtoLens.Tag 3, payload__field_descriptor),
                    (Data.ProtoLens.Tag 4, fillUsername__field_descriptor),
                    (Data.ProtoLens.Tag 5, fillOauthScope__field_descriptor),
                    (Data.ProtoLens.Tag 6, responseCompressed__field_descriptor),
                    (Data.ProtoLens.Tag 7, responseStatus__field_descriptor),
                    (Data.ProtoLens.Tag 8, expectCompressed__field_descriptor)])
                (Data.Map.fromList
                   [("response_type", responseType__field_descriptor),
                    ("response_size", responseSize__field_descriptor),
                    ("payload", payload__field_descriptor),
                    ("fill_username", fillUsername__field_descriptor),
                    ("fill_oauth_scope", fillOauthScope__field_descriptor),
                    ("response_compressed", responseCompressed__field_descriptor),
                    ("response_status", responseStatus__field_descriptor),
                    ("expect_compressed", expectCompressed__field_descriptor)])

data SimpleResponse = SimpleResponse{_SimpleResponse'payload ::
                                     Prelude.Maybe Payload,
                                     _SimpleResponse'username :: Data.Text.Text,
                                     _SimpleResponse'oauthScope :: Data.Text.Text}
                    deriving (Prelude.Show, Prelude.Eq)

type instance Data.ProtoLens.Field "payload" SimpleResponse =
     Payload

instance Data.ProtoLens.HasField "payload" SimpleResponse
         SimpleResponse where
        field _
          = (Prelude..) maybe'payload
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance Data.ProtoLens.Field "maybe'payload" SimpleResponse =
     Prelude.Maybe Payload

instance Data.ProtoLens.HasField "maybe'payload" SimpleResponse
         SimpleResponse where
        field _
          = Lens.Family2.Unchecked.lens _SimpleResponse'payload
              (\ x__ y__ -> x__{_SimpleResponse'payload = y__})

type instance Data.ProtoLens.Field "username" SimpleResponse =
     Data.Text.Text

instance Data.ProtoLens.HasField "username" SimpleResponse
         SimpleResponse where
        field _
          = Lens.Family2.Unchecked.lens _SimpleResponse'username
              (\ x__ y__ -> x__{_SimpleResponse'username = y__})

type instance Data.ProtoLens.Field "oauthScope" SimpleResponse =
     Data.Text.Text

instance Data.ProtoLens.HasField "oauthScope" SimpleResponse
         SimpleResponse where
        field _
          = Lens.Family2.Unchecked.lens _SimpleResponse'oauthScope
              (\ x__ y__ -> x__{_SimpleResponse'oauthScope = y__})

instance Data.Default.Class.Default SimpleResponse where
        def
          = SimpleResponse{_SimpleResponse'payload = Prelude.Nothing,
                           _SimpleResponse'username = Data.ProtoLens.fieldDefault,
                           _SimpleResponse'oauthScope = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message SimpleResponse where
        descriptor
          = let payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Payload)
                      (Data.ProtoLens.OptionalField maybe'payload)
                username__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "username"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional username)
                oauthScope__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "oauth_scope"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional oauthScope)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, payload__field_descriptor),
                    (Data.ProtoLens.Tag 2, username__field_descriptor),
                    (Data.ProtoLens.Tag 3, oauthScope__field_descriptor)])
                (Data.Map.fromList
                   [("payload", payload__field_descriptor),
                    ("username", username__field_descriptor),
                    ("oauth_scope", oauthScope__field_descriptor)])

data StreamingInputCallRequest = StreamingInputCallRequest{_StreamingInputCallRequest'payload
                                                           :: Prelude.Maybe Payload,
                                                           _StreamingInputCallRequest'expectCompressed
                                                           :: Prelude.Maybe BoolValue}
                               deriving (Prelude.Show, Prelude.Eq)

type instance
     Data.ProtoLens.Field "payload" StreamingInputCallRequest = Payload

instance Data.ProtoLens.HasField "payload"
         StreamingInputCallRequest StreamingInputCallRequest where
        field _
          = (Prelude..) maybe'payload
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'payload" StreamingInputCallRequest =
     Prelude.Maybe Payload

instance Data.ProtoLens.HasField "maybe'payload"
         StreamingInputCallRequest StreamingInputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens _StreamingInputCallRequest'payload
              (\ x__ y__ -> x__{_StreamingInputCallRequest'payload = y__})

type instance
     Data.ProtoLens.Field "expectCompressed" StreamingInputCallRequest =
     BoolValue

instance Data.ProtoLens.HasField "expectCompressed"
         StreamingInputCallRequest StreamingInputCallRequest where
        field _
          = (Prelude..) maybe'expectCompressed
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'expectCompressed"
       StreamingInputCallRequest
     = Prelude.Maybe BoolValue

instance Data.ProtoLens.HasField "maybe'expectCompressed"
         StreamingInputCallRequest StreamingInputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens
              _StreamingInputCallRequest'expectCompressed
              (\ x__ y__ ->
                 x__{_StreamingInputCallRequest'expectCompressed = y__})

instance Data.Default.Class.Default StreamingInputCallRequest where
        def
          = StreamingInputCallRequest{_StreamingInputCallRequest'payload =
                                        Prelude.Nothing,
                                      _StreamingInputCallRequest'expectCompressed = Prelude.Nothing}

instance Data.ProtoLens.Message StreamingInputCallRequest where
        descriptor
          = let payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Payload)
                      (Data.ProtoLens.OptionalField maybe'payload)
                expectCompressed__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "expect_compressed"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor BoolValue)
                      (Data.ProtoLens.OptionalField maybe'expectCompressed)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, payload__field_descriptor),
                    (Data.ProtoLens.Tag 2, expectCompressed__field_descriptor)])
                (Data.Map.fromList
                   [("payload", payload__field_descriptor),
                    ("expect_compressed", expectCompressed__field_descriptor)])

data StreamingInputCallResponse = StreamingInputCallResponse{_StreamingInputCallResponse'aggregatedPayloadSize
                                                             :: Data.Int.Int32}
                                deriving (Prelude.Show, Prelude.Eq)

type instance
     Data.ProtoLens.Field "aggregatedPayloadSize"
       StreamingInputCallResponse
     = Data.Int.Int32

instance Data.ProtoLens.HasField "aggregatedPayloadSize"
         StreamingInputCallResponse StreamingInputCallResponse where
        field _
          = Lens.Family2.Unchecked.lens
              _StreamingInputCallResponse'aggregatedPayloadSize
              (\ x__ y__ ->
                 x__{_StreamingInputCallResponse'aggregatedPayloadSize = y__})

instance Data.Default.Class.Default StreamingInputCallResponse
         where
        def
          = StreamingInputCallResponse{_StreamingInputCallResponse'aggregatedPayloadSize
                                         = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message StreamingInputCallResponse where
        descriptor
          = let aggregatedPayloadSize__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "aggregated_payload_size"
                      (Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         aggregatedPayloadSize)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, aggregatedPayloadSize__field_descriptor)])
                (Data.Map.fromList
                   [("aggregated_payload_size",
                     aggregatedPayloadSize__field_descriptor)])

data StreamingOutputCallRequest = StreamingOutputCallRequest{_StreamingOutputCallRequest'responseType
                                                             :: PayloadType,
                                                             _StreamingOutputCallRequest'responseParameters
                                                             :: [ResponseParameters],
                                                             _StreamingOutputCallRequest'payload ::
                                                             Prelude.Maybe Payload,
                                                             _StreamingOutputCallRequest'responseStatus
                                                             :: Prelude.Maybe EchoStatus}
                                deriving (Prelude.Show, Prelude.Eq)

type instance
     Data.ProtoLens.Field "responseType" StreamingOutputCallRequest =
     PayloadType

instance Data.ProtoLens.HasField "responseType"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens
              _StreamingOutputCallRequest'responseType
              (\ x__ y__ -> x__{_StreamingOutputCallRequest'responseType = y__})

type instance
     Data.ProtoLens.Field "responseParameters"
       StreamingOutputCallRequest
     = [ResponseParameters]

instance Data.ProtoLens.HasField "responseParameters"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens
              _StreamingOutputCallRequest'responseParameters
              (\ x__ y__ ->
                 x__{_StreamingOutputCallRequest'responseParameters = y__})

type instance
     Data.ProtoLens.Field "payload" StreamingOutputCallRequest = Payload

instance Data.ProtoLens.HasField "payload"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = (Prelude..) maybe'payload
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'payload" StreamingOutputCallRequest =
     Prelude.Maybe Payload

instance Data.ProtoLens.HasField "maybe'payload"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens _StreamingOutputCallRequest'payload
              (\ x__ y__ -> x__{_StreamingOutputCallRequest'payload = y__})

type instance
     Data.ProtoLens.Field "responseStatus" StreamingOutputCallRequest =
     EchoStatus

instance Data.ProtoLens.HasField "responseStatus"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = (Prelude..) maybe'responseStatus
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'responseStatus"
       StreamingOutputCallRequest
     = Prelude.Maybe EchoStatus

instance Data.ProtoLens.HasField "maybe'responseStatus"
         StreamingOutputCallRequest StreamingOutputCallRequest where
        field _
          = Lens.Family2.Unchecked.lens
              _StreamingOutputCallRequest'responseStatus
              (\ x__ y__ ->
                 x__{_StreamingOutputCallRequest'responseStatus = y__})

instance Data.Default.Class.Default StreamingOutputCallRequest
         where
        def
          = StreamingOutputCallRequest{_StreamingOutputCallRequest'responseType
                                         = Data.Default.Class.def,
                                       _StreamingOutputCallRequest'responseParameters = [],
                                       _StreamingOutputCallRequest'payload = Prelude.Nothing,
                                       _StreamingOutputCallRequest'responseStatus = Prelude.Nothing}

instance Data.ProtoLens.Message StreamingOutputCallRequest where
        descriptor
          = let responseType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_type"
                      (Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor PayloadType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional responseType)
                responseParameters__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_parameters"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor ResponseParameters)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         responseParameters)
                payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Payload)
                      (Data.ProtoLens.OptionalField maybe'payload)
                responseStatus__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response_status"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor EchoStatus)
                      (Data.ProtoLens.OptionalField maybe'responseStatus)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, responseType__field_descriptor),
                    (Data.ProtoLens.Tag 2, responseParameters__field_descriptor),
                    (Data.ProtoLens.Tag 3, payload__field_descriptor),
                    (Data.ProtoLens.Tag 7, responseStatus__field_descriptor)])
                (Data.Map.fromList
                   [("response_type", responseType__field_descriptor),
                    ("response_parameters", responseParameters__field_descriptor),
                    ("payload", payload__field_descriptor),
                    ("response_status", responseStatus__field_descriptor)])

data StreamingOutputCallResponse = StreamingOutputCallResponse{_StreamingOutputCallResponse'payload
                                                               :: Prelude.Maybe Payload}
                                 deriving (Prelude.Show, Prelude.Eq)

type instance
     Data.ProtoLens.Field "payload" StreamingOutputCallResponse =
     Payload

instance Data.ProtoLens.HasField "payload"
         StreamingOutputCallResponse StreamingOutputCallResponse where
        field _
          = (Prelude..) maybe'payload
              (Data.ProtoLens.maybeLens Data.Default.Class.def)

type instance
     Data.ProtoLens.Field "maybe'payload" StreamingOutputCallResponse =
     Prelude.Maybe Payload

instance Data.ProtoLens.HasField "maybe'payload"
         StreamingOutputCallResponse StreamingOutputCallResponse where
        field _
          = Lens.Family2.Unchecked.lens _StreamingOutputCallResponse'payload
              (\ x__ y__ -> x__{_StreamingOutputCallResponse'payload = y__})

instance Data.Default.Class.Default StreamingOutputCallResponse
         where
        def
          = StreamingOutputCallResponse{_StreamingOutputCallResponse'payload
                                          = Prelude.Nothing}

instance Data.ProtoLens.Message StreamingOutputCallResponse where
        descriptor
          = let payload__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "payload"
                      (Data.ProtoLens.MessageField ::
                         Data.ProtoLens.FieldTypeDescriptor Payload)
                      (Data.ProtoLens.OptionalField maybe'payload)
              in
              Data.ProtoLens.MessageDescriptor
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, payload__field_descriptor)])
                (Data.Map.fromList [("payload", payload__field_descriptor)])

aggregatedPayloadSize ::
                      forall msg msg' .
                        Data.ProtoLens.HasField "aggregatedPayloadSize" msg msg' =>
                        Lens.Family2.Lens msg msg'
                          (Data.ProtoLens.Field "aggregatedPayloadSize" msg)
                          (Data.ProtoLens.Field "aggregatedPayloadSize" msg')
aggregatedPayloadSize
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "aggregatedPayloadSize")

backoffMs ::
          forall msg msg' . Data.ProtoLens.HasField "backoffMs" msg msg' =>
            Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "backoffMs" msg)
              (Data.ProtoLens.Field "backoffMs" msg')
backoffMs
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "backoffMs")

body ::
     forall msg msg' . Data.ProtoLens.HasField "body" msg msg' =>
       Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "body" msg)
         (Data.ProtoLens.Field "body" msg')
body
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "body")

code ::
     forall msg msg' . Data.ProtoLens.HasField "code" msg msg' =>
       Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "code" msg)
         (Data.ProtoLens.Field "code" msg')
code
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "code")

compressed ::
           forall msg msg' . Data.ProtoLens.HasField "compressed" msg msg' =>
             Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "compressed" msg)
               (Data.ProtoLens.Field "compressed" msg')
compressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "compressed")

expectCompressed ::
                 forall msg msg' .
                   Data.ProtoLens.HasField "expectCompressed" msg msg' =>
                   Lens.Family2.Lens msg msg'
                     (Data.ProtoLens.Field "expectCompressed" msg)
                     (Data.ProtoLens.Field "expectCompressed" msg')
expectCompressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "expectCompressed")

fillOauthScope ::
               forall msg msg' .
                 Data.ProtoLens.HasField "fillOauthScope" msg msg' =>
                 Lens.Family2.Lens msg msg'
                   (Data.ProtoLens.Field "fillOauthScope" msg)
                   (Data.ProtoLens.Field "fillOauthScope" msg')
fillOauthScope
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "fillOauthScope")

fillUsername ::
             forall msg msg' .
               Data.ProtoLens.HasField "fillUsername" msg msg' =>
               Lens.Family2.Lens msg msg'
                 (Data.ProtoLens.Field "fillUsername" msg)
                 (Data.ProtoLens.Field "fillUsername" msg')
fillUsername
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "fillUsername")

intervalUs ::
           forall msg msg' . Data.ProtoLens.HasField "intervalUs" msg msg' =>
             Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "intervalUs" msg)
               (Data.ProtoLens.Field "intervalUs" msg')
intervalUs
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "intervalUs")

maxReconnectBackoffMs ::
                      forall msg msg' .
                        Data.ProtoLens.HasField "maxReconnectBackoffMs" msg msg' =>
                        Lens.Family2.Lens msg msg'
                          (Data.ProtoLens.Field "maxReconnectBackoffMs" msg)
                          (Data.ProtoLens.Field "maxReconnectBackoffMs" msg')
maxReconnectBackoffMs
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maxReconnectBackoffMs")

maybe'compressed ::
                 forall msg msg' .
                   Data.ProtoLens.HasField "maybe'compressed" msg msg' =>
                   Lens.Family2.Lens msg msg'
                     (Data.ProtoLens.Field "maybe'compressed" msg)
                     (Data.ProtoLens.Field "maybe'compressed" msg')
maybe'compressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maybe'compressed")

maybe'expectCompressed ::
                       forall msg msg' .
                         Data.ProtoLens.HasField "maybe'expectCompressed" msg msg' =>
                         Lens.Family2.Lens msg msg'
                           (Data.ProtoLens.Field "maybe'expectCompressed" msg)
                           (Data.ProtoLens.Field "maybe'expectCompressed" msg')
maybe'expectCompressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maybe'expectCompressed")

maybe'payload ::
              forall msg msg' .
                Data.ProtoLens.HasField "maybe'payload" msg msg' =>
                Lens.Family2.Lens msg msg'
                  (Data.ProtoLens.Field "maybe'payload" msg)
                  (Data.ProtoLens.Field "maybe'payload" msg')
maybe'payload
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maybe'payload")

maybe'responseCompressed ::
                         forall msg msg' .
                           Data.ProtoLens.HasField "maybe'responseCompressed" msg msg' =>
                           Lens.Family2.Lens msg msg'
                             (Data.ProtoLens.Field "maybe'responseCompressed" msg)
                             (Data.ProtoLens.Field "maybe'responseCompressed" msg')
maybe'responseCompressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maybe'responseCompressed")

maybe'responseStatus ::
                     forall msg msg' .
                       Data.ProtoLens.HasField "maybe'responseStatus" msg msg' =>
                       Lens.Family2.Lens msg msg'
                         (Data.ProtoLens.Field "maybe'responseStatus" msg)
                         (Data.ProtoLens.Field "maybe'responseStatus" msg')
maybe'responseStatus
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "maybe'responseStatus")

message ::
        forall msg msg' . Data.ProtoLens.HasField "message" msg msg' =>
          Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "message" msg)
            (Data.ProtoLens.Field "message" msg')
message
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "message")

oauthScope ::
           forall msg msg' . Data.ProtoLens.HasField "oauthScope" msg msg' =>
             Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "oauthScope" msg)
               (Data.ProtoLens.Field "oauthScope" msg')
oauthScope
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "oauthScope")

passed ::
       forall msg msg' . Data.ProtoLens.HasField "passed" msg msg' =>
         Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "passed" msg)
           (Data.ProtoLens.Field "passed" msg')
passed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "passed")

payload ::
        forall msg msg' . Data.ProtoLens.HasField "payload" msg msg' =>
          Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "payload" msg)
            (Data.ProtoLens.Field "payload" msg')
payload
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "payload")

responseCompressed ::
                   forall msg msg' .
                     Data.ProtoLens.HasField "responseCompressed" msg msg' =>
                     Lens.Family2.Lens msg msg'
                       (Data.ProtoLens.Field "responseCompressed" msg)
                       (Data.ProtoLens.Field "responseCompressed" msg')
responseCompressed
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "responseCompressed")

responseParameters ::
                   forall msg msg' .
                     Data.ProtoLens.HasField "responseParameters" msg msg' =>
                     Lens.Family2.Lens msg msg'
                       (Data.ProtoLens.Field "responseParameters" msg)
                       (Data.ProtoLens.Field "responseParameters" msg')
responseParameters
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "responseParameters")

responseSize ::
             forall msg msg' .
               Data.ProtoLens.HasField "responseSize" msg msg' =>
               Lens.Family2.Lens msg msg'
                 (Data.ProtoLens.Field "responseSize" msg)
                 (Data.ProtoLens.Field "responseSize" msg')
responseSize
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "responseSize")

responseStatus ::
               forall msg msg' .
                 Data.ProtoLens.HasField "responseStatus" msg msg' =>
                 Lens.Family2.Lens msg msg'
                   (Data.ProtoLens.Field "responseStatus" msg)
                   (Data.ProtoLens.Field "responseStatus" msg')
responseStatus
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym ::
         Data.ProtoLens.ProxySym "responseStatus")

responseType ::
             forall msg msg' .
               Data.ProtoLens.HasField "responseType" msg msg' =>
               Lens.Family2.Lens msg msg'
                 (Data.ProtoLens.Field "responseType" msg)
                 (Data.ProtoLens.Field "responseType" msg')
responseType
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "responseType")

size ::
     forall msg msg' . Data.ProtoLens.HasField "size" msg msg' =>
       Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "size" msg)
         (Data.ProtoLens.Field "size" msg')
size
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "size")

type' ::
      forall msg msg' . Data.ProtoLens.HasField "type'" msg msg' =>
        Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "type'" msg)
          (Data.ProtoLens.Field "type'" msg')
type'
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "type'")

username ::
         forall msg msg' . Data.ProtoLens.HasField "username" msg msg' =>
           Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "username" msg)
             (Data.ProtoLens.Field "username" msg')
username
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "username")

value ::
      forall msg msg' . Data.ProtoLens.HasField "value" msg msg' =>
        Lens.Family2.Lens msg msg' (Data.ProtoLens.Field "value" msg)
          (Data.ProtoLens.Field "value" msg')
value
  = Data.ProtoLens.field
      (Data.ProtoLens.ProxySym :: Data.ProtoLens.ProxySym "value")