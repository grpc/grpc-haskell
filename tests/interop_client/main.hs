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

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class                (liftIO)
import           Control.Concurrent                    (threadDelay)
import           Data.Monoid                           ((<>))
import           Data.Either                           (either)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.Mem

import qualified Data.ByteString                       as B
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy                  as L

import           Network.Grpc.Core.Call
import           Network.Grpc.Lib.Core
import           Network.Grpc.Lib.Metadata

import           Data.Default.Class                    (def)
import           Data.ProtoLens                        (decodeMessage,
                                                        encodeMessage)
import           Proto.Src.Proto.Grpc.Testing.Messages (Payload (..),
                                                        ResponseParameters (..),
                                                        SimpleRequest (..),
                                                        SimpleResponse (..),
                                                        StreamingOutputCallRequest (..),
                                                        StreamingInputCallRequest (..),
                                                        StreamingInputCallResponse (..),
                                                        StreamingOutputCallResponse (..),
                                                        EchoStatus (..))

data Options = Options
  { optServerHost            :: String
  , optServerHostOverride    :: String
  , optServerPort            :: Int
  , optTestCase              :: TestCaseFlag
  , optUseTLS                :: Bool
  , optUseTestCA             :: Bool
  , optDefaultServiceAccount :: String
  , optOAuthScope            :: String
  , optServiceAccountKeyFile :: FilePath
}

data TestCaseFlag
  = TestCase TestCase
  | AllTests
  | TestCaseUnknown String

data TestCase
  = CancelAfterBegin
  | ClientStreaming
  | ServerStreaming
  | ServerStreamingWithSlowConsumer
  | CustomMetadata
  | EmptyStream
  | EmptyUnary
  | LargeUnary
  | PingPong
  | StatusCodeAndMessage
  | TimeoutOnSleepingServer
  | UnimplementedMethod
  | UnimplementedService
  deriving (Bounded, Enum, Show)

defaultOptions :: Options
defaultOptions = Options
  { optServerHost = ""
  , optServerHostOverride = ""
  , optServerPort = 0
  , optTestCase = TestCaseUnknown "not specified"
  , optUseTLS = False
  , optUseTestCA = False
  , optDefaultServiceAccount = ""
  , optOAuthScope = ""
  , optServiceAccountKeyFile = ""
  }

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool "TRUE" = True
stringToBool _      = False

testCaseMap :: [(String, (Bool, TestCase))]
testCaseMap =
 [ ("cancel_after_begin"        , (True,  CancelAfterBegin))
 , ("client_streaming"          , (True,  ClientStreaming))
 , ("server_streaming"          , (True,  ServerStreaming))
 , ("slow_consumer"             , (False, ServerStreamingWithSlowConsumer))
 , ("custom_metadata"           , (True,  CustomMetadata))
 , ("empty_stream"              , (True,  EmptyStream))
 , ("empty_unary"               , (True,  EmptyUnary))
 , ("large_unary"               , (True,  LargeUnary))
 , ("ping_pong"                 , (True,  PingPong))
 , ("status_code_and_message"   , (True,  StatusCodeAndMessage))
 , ("timeout_on_sleeping_server", (True,  TimeoutOnSleepingServer))
 , ("unimplemented_method"      , (True,  UnimplementedMethod))
 , ("unimplemented_service"     , (True,  UnimplementedService))
 ]

allTests :: [TestCase]
allTests = [ tc | (_, (True, tc)) <- testCaseMap ]

renderTestCases :: String
renderTestCases = unlines (map (" - " ++ ) ("all":map fst testCaseMap))

testCase :: String -> TestCaseFlag
testCase "all"                             = AllTests
testCase str
  | Just (_, tc) <- lookup str testCaseMap = TestCase tc
testCase unknown                           = TestCaseUnknown unknown

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["server_host"]
      (ReqArg (\host opts -> opts { optServerHost = host }) "HOSTNAME")
      "The server host to connect to. For example, \"localhost\" or \"127.0.0.1\""
  , Option [] ["server_host_override"]
      (ReqArg (\host opts -> opts { optServerHostOverride = host}) "HOSTNAME")
      ("The server host to claim to be connecting to, for use in TLS and HTTP/2 :authority header.\n"
      ++ "If unspecified, the value of --server_host will be used")
  , Option [] ["server_port"]
      (ReqArg (\port opts -> opts { optServerPort = read port }) "PORT")
      "The server port to connect to. For example, \"8080\""
  , Option [] ["test_case"]
      (ReqArg (\test opts -> opts { optTestCase = testCase test }) "TESTCASE")
      ("The name of the test case to execute. Test cases;\n" ++ renderTestCases)
  , Option [] ["use_tls"]
      (ReqArg (\tls opts -> opts { optUseTLS = stringToBool tls }) "BOOLEAN")
      "Whether to use a plaintext or encrypted connection"
  , Option [] ["use_test_ca"]
      (ReqArg (\test_ca opts -> opts { optUseTestCA = stringToBool test_ca }) "BOOLEAN")
      "Whether to replace platform root CAs with ca.pem as the CA root"
  , Option [] ["default_service_account"]
      (ReqArg (\acc opts -> opts { optDefaultServiceAccount = acc }) "ACCOUNT_EMAIL")
      "Email of the GCE default service account."
  , Option [] ["oauth_scope"]
      (ReqArg (\scope opts -> opts { optOAuthScope = scope }) "SCOPE")
      "OAuth scope. For example, \"https://www.googleapis.com/auth/xapi.zoo\""
  , Option [] ["service_account_key_file"]
      (ReqArg (\file opts -> opts { optServiceAccountKeyFile = file }) "PATH")
      ("The path to the service account JSON key file generated from GCE developer console.\n"
      ++ "Clients must support TLS with ALPN. Clients must not disable certificate checking.")
  ]

parseOptions :: [String] -> Either [String] Options
parseOptions flags =
  case getOpt Permute options flags of
    (opts, [], []) ->
      let opts' = foldl (flip id) defaultOptions opts in
        case validateOptions opts' of
            [] -> Right opts'
            errs -> Left errs
    (_, unkn, err) -> Left (map ("unrecognized argument " ++) unkn ++ err)

validateOptions :: Options -> [String]
validateOptions opts =
  [ "Missing flag --server_host=HOSTNAME" | "" <- return (optServerHost opts) ]
  ++ [ "Missing flag --server_port=PORT" | 0 <- return (optServerPort opts) ]
  ++ [ "Missing flag --test_case=TESTCASE or invalid test case" | TestCaseUnknown _ <- return (optTestCase opts) ]

callOptions :: CallOptions
callOptions = withRelativeDeadlineSeconds 2

main :: IO ()
main = do
  args <- getArgs
  opts <- case parseOptions args of
    Left errs -> do
      putStrLn "Errors while parsing flags:"
      mapM_ putStrLn errs
      putStrLn ""
      putStrLn (usageInfo "Usage: interop_client [OPTION]" options)
      exitFailure
    Right opts -> return opts
  ok <- case optTestCase opts of
    AllTests ->
      fmap and $ forM allTests $ \tc ->
        testWrapper tc (runTest tc opts)
    TestCaseUnknown tc -> do
      putStrLn ("Unknown or not specified test case: " ++ tc)
      return False
    TestCase tc ->
      testWrapper tc (runTest tc opts)
  unless ok exitFailure

testWrapper :: TestCase -> IO (Either String ()) -> IO Bool
testWrapper tc act =
  bracket_
    grpcInit
    (performMajorGC >> grpcShutdown)
    (do
      result <- act
      case result of
        Right _ -> do
          putStrLn (show tc ++ ": ok")
          return True
        Left err -> do
          putStrLn (show tc ++ ": failed; " ++ err)
          return False)

runTest :: TestCase -> Options -> IO (Either String ())
runTest CancelAfterBegin = runCancelAfterBeginTest
runTest ClientStreaming = runClientStreamingTest
runTest ServerStreaming = runServerStreamingTest
runTest ServerStreamingWithSlowConsumer = runServerStreamingWithSlowConsumerTest
runTest CustomMetadata = runCustomMetadataTest
runTest EmptyStream = runEmptyStreamTest
runTest EmptyUnary = runEmptyUnaryTest
runTest LargeUnary = runLargeUnaryTest
runTest PingPong = runPingPongTest
runTest StatusCodeAndMessage = runStatusCodeAndMessageTest
runTest TimeoutOnSleepingServer = runTimeoutOnSleepingServerTest
runTest UnimplementedMethod = runUnimplementedMethodTest
runTest UnimplementedService = runUnimplementedServiceTest

newChannel :: Options -> IO Channel
newChannel opts =
  createInsecureChannel (C8.pack (optServerHost opts)) (optServerPort opts) mempty

seq_ :: [(String, IO (Either String ()))] -> IO (Either String ())
seq_ [] = return (Right ())
seq_ ((msg, x):xs) = do
  v <- x
  case v of
    Right _ -> seq_ xs
    Left desc -> return (Left (msg ++ ": " ++ desc))

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft left = either (Left . left) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x

-- | This test verifies that implementations support zero-size messages.
-- Ideally, client implementations would verify that the request and
-- response were zero bytes serialized, but this is generally prohibitive to
-- perform, so is not required.
--
-- Server features:
--  - EmptyCall
-- Procedure:
--  1. Client calls EmptyCall with the default Empty message
-- Client asserts:
--  - call was successful
--  - response is non-null
--
-- It may be possible to use UnaryCall instead of EmptyCall, but it is harder
-- to ensure that the proto serialized to zero bytes.
runEmptyUnaryTest :: Options -> IO (Either String ())
runEmptyUnaryTest opts =
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx callOptions "/grpc.testing.TestService/EmptyCall" B.empty
      case resp of
        RpcOk (UnaryResult _ _ msg)
          | L.null msg -> return (Right ())
          | otherwise -> return (Left "Non zero reply, failure.")
        RpcError err -> return (Left (show err))

-- | This test verifies unary calls succeed in sending messages, and touches
-- on flow control (even if compression is enabled on the channel).
-- Server features:
--  - UnaryCall
-- Procedure:
--  1. Client calls UnaryCall with:
--     {
--       response_size: 314159
--       payload:{
--         body: 271828 bytes of zeros
--       }
--     }
-- Client asserts:
--  - call was successful
--  - response payload body is 314159 bytes in size
--  - clients are free to assert that the response payload body contents are
--      zero and comparing the entire response message against a golden
--      response.
runLargeUnaryTest :: Options -> IO (Either String ())
runLargeUnaryTest opts = do
  let req = def { _SimpleRequest'responseSize = 314159
                , _SimpleRequest'payload = Just def {
                    _Payload'body = B.replicate 271828 0
                  }
                }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx callOptions "/grpc.testing.TestService/UnaryCall" (encodeMessage req)
      case resp of
        RpcOk (UnaryResult _ _ resp') ->
          case decodeMessage (L.toStrict resp') of
            Left err -> return (Left ("proto decoder says: " ++ err))
            Right msg ->
              case _SimpleResponse'payload msg of
                Nothing -> return (Left "no payload")
                Just payload ->
                  case B.length (_Payload'body payload) of
                    314159 -> return (Right ())
                    n      -> return (Left ("wrong payload: " ++ show n))
        RpcError err -> return (Left (show err))


-- | This test verifies that custom metadata in either binary or ascii format can be
-- sent as initial-metadata by the client and as both initial- and trailing-metadata
-- by the server.
-- Server features:
--  - UnaryCall
--  - FullDuplexCall
--  - Echo Metadata
-- Procedure:
--  1. The client attaches custom metadata with the following keys and values:
--     ```
--     key: "x-grpc-test-echo-initial", value: "test_initial_metadata_value"
--     key: "x-grpc-test-echo-trailing-bin", value: 0xababab
--     ```
--     to a UnaryCall with request:
--     ```
--     {
--       response_size: 314159
--       payload:{
--         body: 271828 bytes of zeros
--       }
--     }
--     ```
--  2. The client attaches custom metadata with the following keys and values:
--     ```
--     key: "x-grpc-test-echo-initial", value: "test_initial_metadata_value"
--     key: "x-grpc-test-echo-trailing-bin", value: 0xababab
--     ```
--     to a FullDuplexCall with request:
--     ```
--     {
--       response_size: 314159
--       payload:{
--         body: 271828 bytes of zeros
--       }
--     }
--     ```
--     and then half-closes
--
-- Client asserts:
--  - call was successful
--  - metadata with key `"x-grpc-test-echo-initial"` and value
--      `"test_initial_metadata_value"`is received in the initial metadata for calls
--      in Procedure steps 1 and 2.
--  - metadata with key `"x-grpc-test-echo-trailing-bin"` and value `0xababab` is
--      received in the trailing metadata for calls in Procedure steps 1 and 2.
runCustomMetadataTest :: Options -> IO (Either String ())
runCustomMetadataTest opts =
  seq_
    [ ("procedure1", procedure1)
    , ("procedure2", procedure2)]
  where
    expectedInitMd = Metadata "x-grpc-test-echo-initial" "test_initial_metadata_value" 0
    expectedTrailMd = Metadata "x-grpc-test-echo-trailing-bin" "\x0a\x0b\x0a\x0b\x0a\x0b" 0
    metadata = [ expectedInitMd, expectedTrailMd ]
    callOptions' = callOptions <> withMetadata metadata

    checkMetadata :: [Metadata] -> [Metadata] -> IO (Either String ())
    checkMetadata initMd trailMd
      | initMd /= [expectedInitMd] = return (Left ("wrong initial metadata, got " ++ show initMd))
      | trailMd /= [expectedTrailMd] = return (Left ("wrong trailing metadata, got " ++ show trailMd))
      | otherwise = return (Right ())

    procedure1 :: IO (Either String ())
    procedure1 = do
      let
        req = def { _SimpleRequest'responseSize = 314159
                  , _SimpleRequest'payload = Just def {
                      _Payload'body = B.replicate 271828 0 }
                  }
      bracket (newChannel opts) destroyChannel $ \channel ->
        bracket (newClientContext channel) destroyClientContext $ \ctx -> do
          resp <- callUnary ctx callOptions' "/grpc.testing.TestService/UnaryCall" (encodeMessage req)
          case resp of
            RpcOk (UnaryResult initMd trailMd _) ->
              checkMetadata initMd trailMd
            RpcError err ->
              return (Left (show err))

    procedure2 :: IO (Either String ())
    procedure2 = do
      let
        req = def { _StreamingOutputCallRequest'responseParameters = [ def { _ResponseParameters'size = 314159 } ]
                  , _StreamingOutputCallRequest'payload = Just def { _Payload'body = B.replicate 271828 0 }
                  }
      bracket (newChannel opts) destroyChannel $ \channel ->
        bracket (newClientContext channel) destroyClientContext $ \ctx -> do
          mds <- runRpc $ do
            RpcOk client <- liftIO (callBidi ctx callOptions' "/grpc.testing.TestService/FullDuplexCall")
            sendMessage client (encodeMessage req)
            sendHalfClose client
            _ <- receiveMessage client
            initMd <- initialMetadata client
            (RpcStatus trailMd _ _) <- waitForStatus client
            closeCall client
            return (initMd, trailMd)
          case mds of
            RpcOk (initMd, trailMd) ->
              checkMetadata initMd trailMd
            RpcError err ->
              return (Left (show err))

-- | This test verifies calling unimplemented RPC method returns the UNIMPLEMENTED
-- status code.
-- Server features: N/A
-- Procedure:
--  1. Client calls `grpc.testing.TestService/UnimplementedCall` with an empty
--     request (defined as `grpc.testing.Empty`):
-- Client asserts:
--  - received status code is 12 (UNIMPLEMENTED)
runUnimplementedMethodTest :: Options -> IO (Either String ())
runUnimplementedMethodTest opts =
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx callOptions "/grpc.testing.TestService/UnimplementedCall" B.empty
      case resp of
        RpcError (StatusError StatusUnimplemented _) -> return (Right ())
        RpcError err -> return (Left ("RPC failed with the wrong error, got " ++ show err))
        RpcOk _ -> return (Left "RPC succeeded, it should have failed.")

-- | This test verifies calling an unimplemented server returns the
-- UNIMPLEMENTED status code.
--
-- Server features: N/A
--
-- Procedure:
-- 1. Client calls `grpc.testing.UnimplementedService/UnimplementedCall` with an
--    empty request (defined as `grpc.testing.Empty`)
--
-- Client asserts:
-- 1. received status code is 12 (UNIMPLEMENTED)
runUnimplementedServiceTest :: Options -> IO (Either String ())
runUnimplementedServiceTest opts =
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx callOptions "/grpc.testing.UnimplementedService/UnimplementedCall" B.empty
      case resp of
        RpcError (StatusError StatusUnimplemented _) -> return (Right ())
        RpcError err -> return (Left ("RPC failed with the wrong error, got " ++ show err))
        RpcOk _ -> return (Left "RPC succeeded, it should have failed.")

-- | This test verifies that a request can be cancelled after metadata
-- has been sent but before payloads are sent.
--
-- Server features:
--  1. [StreamingInputCall][]
--
-- Procedure:
--  1. Client starts StreamingInputCall
--  2. Client immediately cancels request
--
-- Client asserts:
--  - Call completed with status CANCELLED
runCancelAfterBeginTest :: Options -> IO (Either String ())
runCancelAfterBeginTest opts =
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callUpstream ctx callOptions "/grpc.testing.TestService/StreamingInputCall")
        cancelCall client
        closeCall client
      case resp of
        RpcError (StatusError StatusCancelled _) ->
          return (Right ())
        resp' ->
          return (Left ("Wanted StatusCancelled, got=" ++ show resp'))

-- | This test verifies that an RPC request whose lifetime exceeds its configured
-- timeout value will end with the DeadlineExceeded status.
--
-- Server features:
-- * [FullDuplexCall][]
--
-- Procedure:
--  1. Client calls FullDuplexCall with the following request and sets its timeout
--     to 1ms
--
--     ```
--     {
--       payload:{
--         body: 27182 bytes of zeros
--       }
--     }
--     ```
--
--  2. Client waits
--
-- Client asserts:
--  - Call completed with status DEADLINE_EXCEEDED.
runTimeoutOnSleepingServerTest :: Options -> IO (Either String ())
runTimeoutOnSleepingServerTest opts = do
  let
    callOptions' = callOptions <> withRelativeDeadlineMillis 1
    req = def { _StreamingOutputCallRequest'payload =
                  Just def { _Payload'body = B.replicate 27182 0 }
              }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callBidi ctx callOptions' "/grpc.testing.TestService/FullDuplexCall")
        sendMessage client (encodeMessage req)
        _ <- waitForStatus client
        closeCall client
      case resp of
        RpcError (StatusError StatusDeadlineExceeded _) ->
          return (Right ())
        RpcError err ->
          return (Left ("got error=" ++ show err ++ ", want=StatusDeadlineExceeded"))
        RpcOk () -> return (Left "got status=OK, want=StatusDeadlineExceeded")
-- |This test verifies that streams support having zero-messages in both
-- directions.
-- Server features:
--  - FullDuplexCall
-- Procedure:
--  1. Client calls FullDuplexCall and then half-closes
-- Client asserts:
--  - call was successful
--  - exactly zero responses
runEmptyStreamTest :: Options -> IO (Either String ())
runEmptyStreamTest opts =
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callBidi ctx callOptions "/grpc.testing.TestService/FullDuplexCall")
        sendHalfClose client
        msgs <- receiveAllMessages client
        closeCall client
        return msgs
      case resp of
        RpcOk msgs
          | null msgs -> return (Right ())
          | otherwise -> return (Left ("expected no messages, got " ++ show msgs))
        RpcError err ->
          return (Left (show err))


-- | This test verifies that client-only streaming succeeds.
--
-- Server features:
--  1. [StreamingInputCall][]
--
-- Procedure:
--  1. Client calls StreamingInputCall
--  2. Client sends:
--
--     ```
--     {
--       payload:{
--         body: 27182 bytes of zeros
--       }
--     }
--     ```
--
--  3. Client then sends:
--
--     ```
--     {
--       payload:{
--         body: 8 bytes of zeros
--       }
--     }
--     ```
--
--  4. Client then sends:
--
--     ```
--     {
--       payload:{
--         body: 1828 bytes of zeros
--       }
--     }
--     ```
--
--  5. Client then sends:
--
--     ```
--     {
--       payload:{
--         body: 45904 bytes of zeros
--       }
--     }
--     ```
--
--  6. Client half-closes
--
-- Client asserts:
--  - call was successful
--  - response aggregated_payload_size is 74922
runClientStreamingTest :: Options -> IO (Either String ())
runClientStreamingTest opts = do
  let
    requestSizes = [27182, 8, 1828, 45904]
    expectedResponseSize = 74922
    aggPayloadSize = _StreamingInputCallResponse'aggregatedPayloadSize
    req n =
      def { _StreamingInputCallRequest'payload = Just def { _Payload'body = B.replicate n 0 }
          }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callUpstream ctx callOptions "/grpc.testing.TestService/StreamingInputCall")
        forM_ requestSizes $ \n ->
          sendMessage client (encodeMessage (req n))
        sendHalfClose client
        msg <- receiveMessage client
        closeCall client
        case maybe (Left "no message") (decodeMessage . L.toStrict) msg of
          Right msg' -> return msg'
          Left err -> fail err
      case resp of
        RpcOk resp'
          | aggPayloadSize resp' == expectedResponseSize ->
              return (Right ())
          | otherwise ->
              return (Left ("aggregated_payload_size=" ++ show (aggPayloadSize resp') ++ ", expected " ++ show expectedResponseSize))
        RpcError err ->
          return (Left (show err))

runServerStreamingTest :: Options -> IO (Either String ())
runServerStreamingTest opts = do
  let
    responseSizes = [31415, 9, 2653, 58979]
    req = def { _StreamingOutputCallRequest'responseParameters =
                  map (\n -> def { _ResponseParameters'size = n }) responseSizes
              }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resps <- runRpc $ do
        RpcOk client <- liftIO (callDownstream ctx callOptions "/grpc.testing.TestService/StreamingOutputCall" (encodeMessage req))
        msgs <- receiveAllMessages client
        closeCall client
        return msgs
      case resps of
        RpcOk resps'
          | length resps' /= length responseSizes ->
            return $ Left ("number of received messages mismatch: " ++ show (length resps'))
          | otherwise -> return $
            forM_ (responseSizes `zip` resps') $ \(expectedSize, resp) -> do
              msg <- mapLeft ("proto decoder says: " ++) (decodeMessage (L.toStrict resp))
              payload <- maybeToEither "no payload" (_StreamingOutputCallResponse'payload msg)
              let
                expectedBody = B.replicate (fromIntegral expectedSize) 0
              unless (_Payload'body payload == expectedBody) $
                Left "payload does not match"
        RpcError err ->
          return (Left (show err))

runServerStreamingWithSlowConsumerTest :: Options -> IO (Either String ())
runServerStreamingWithSlowConsumerTest opts = do
  let
    responseSize = 1030
    responsesCount = 2000
    delaySeconds = 20
    delay = delaySeconds * 1000
    req = def { _StreamingOutputCallRequest'responseParameters =
                  replicate responsesCount def { _ResponseParameters'size = responseSize }
              }
    expectedBody = B.replicate (fromIntegral responseSize) 0
    go client acc = do
      maybeResponse <- receiveMessage client
      case maybeResponse of
        Nothing -> return $ Right (reverse acc)
        Just resp -> do
          let
            eitherMsg = do
              msg <- mapLeft ("proto decoder says: " ++) $ decodeMessage (L.toStrict resp)
              payload <- maybeToEither "no payload" (_StreamingOutputCallResponse'payload msg)
              unless (_Payload'body payload == expectedBody) $
                Left "payload does not match"
              return msg
          case eitherMsg of
            Left err ->
              return (Left err)
            Right msg -> do
              liftIO (threadDelay delay)
              go client (msg : acc)
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callDownstream ctx mempty "/grpc.testing.TestService/StreamingOutputCall" (encodeMessage req))
        msgs <- go client []
        closeCall client
        return msgs
      case resp of
        RpcOk (Right msgs)
          | length msgs == responsesCount ->
            return (Right ())
          | otherwise ->
            return (Left ("responses count does not match: " ++ show (length msgs)))
        RpcOk (Left err) ->
          return (Left err)
        RpcError err ->
          return (Left (show err))

-- | This test verifies unary calls succeed in sending messages, and propagate
-- back status code and message sent along with the messages.
--
-- Server features:
-- * [UnaryCall][]
-- * [FullDuplexCall][]
-- * [Echo Status][]
-- Procedure:
--  1. Client calls UnaryCall with:
--
--     ```
--     {
--       response_status:{
--         code: 2
--         message: "test status message"
--       }
--     }
--     ```
--
--  2. Client calls FullDuplexCall with:
--
--     ```
--     {
--       response_status:{
--         code: 2
--         message: "test status message"
--       }
--     }
--     ```
--
--     and then half-closes
-- Client asserts:
-- * received status code is the same as the sent code for both Procedure steps 1
--   and 2
-- * received status message is the same as the sent message for both Procedure
--   steps 1 and 2
runStatusCodeAndMessageTest :: Options -> IO (Either String ())
runStatusCodeAndMessageTest opts =
  seq_
    [ ("procedure1", runStatusCodeAndMessageTest1 opts)
    , ("procedure2", runStatusCodeAndMessageTest2 opts) ]

runStatusCodeAndMessageTest1 :: Options -> IO (Either String ())
runStatusCodeAndMessageTest1 opts = do
  let
    req = def { _SimpleRequest'responseStatus =
                  Just $ def { _EchoStatus'code = 2
                             , _EchoStatus'message = "test status message"
                             }
              }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx callOptions "/grpc.testing.TestService/UnaryCall" (encodeMessage req)
      case resp of
        RpcError (StatusError StatusUnknown "test status message") -> return (Right ())
        _ -> return (Left ("expected (unknown, \"test status message\"), got= " ++ show resp))

runStatusCodeAndMessageTest2 :: Options -> IO (Either String ())
runStatusCodeAndMessageTest2 opts = do
  let
    req = def { _StreamingOutputCallRequest'responseStatus =
                  Just $ def { _EchoStatus'code = 2
                             , _EchoStatus'message = "test status message"
                             }
              }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      resp <- runRpc $ do
        RpcOk client <- liftIO (callBidi ctx callOptions "/grpc.testing.TestService/FullDuplexCall")
        sendMessage client (encodeMessage req)
        sendHalfClose client
        closeCall client
      case resp of
        RpcError (StatusError StatusUnknown "test status message") -> return (Right ())
        _ -> return (Left ("expected (unknown, \"test status message\"), got= " ++ show resp))

-- | This test verifies that full duplex bidi is supported.
--
-- Server features:
--  1. [FullDuplexCall][]
--
-- Procedure:
--  1. Client calls FullDuplexCall with:
--
--     ```
--     {
--       response_parameters:{
--         size: 31415
--       }
--       payload:{
--         body: 27182 bytes of zeros
--       }
--     }
--     ```
--
--  2. After getting a reply, it sends:
--
--     ```
--     {
--       response_parameters:{
--         size: 9
--       }
--       payload:{
--         body: 8 bytes of zeros
--       }
--     }
--     ```
--
--  3. After getting a reply, it sends:
--
--     ```
--     {
--       response_parameters:{
--         size: 2653
--       }
--       payload:{
--         body: 1828 bytes of zeros
--       }
--     }
--     ```
--
--  4. After getting a reply, it sends:
--
--     ```
--     {
--       response_parameters:{
--         size: 58979
--       }
--       payload:{
--         body: 45904 bytes of zeros
--       }
--     }
--     ```
--
--  5. After getting a reply, client half-closes
--
-- Client asserts:
--  - call was successful
--  - exactly four responses
--  - response payload bodies are sized (in order): 31415, 9, 2653, 58979
--  - clients are free to assert that the response payload body contents are zero
--     and comparing the entire response messages against golden responses
runPingPongTest :: Options -> IO (Either String ())
runPingPongTest opts = do
  let responseSizes = [31415, 9, 2653, 58979]
      payloadSizes =  [27182, 8, 1828, 45904]
      req respSize payloadSize =
        def { _StreamingOutputCallRequest'responseParameters = [ def { _ResponseParameters'size = respSize } ]
            , _StreamingOutputCallRequest'payload = Just def { _Payload'body = B.replicate payloadSize 0 }
            }
  bracket (newChannel opts) destroyChannel $ \channel ->
    bracket (newClientContext channel) destroyClientContext $ \ctx -> do
      mds <- runRpc $ do
        RpcOk client <- liftIO (callBidi ctx callOptions "/grpc.testing.TestService/FullDuplexCall")
        forM_ (zip responseSizes payloadSizes) $ \(respSize, payloadSize) -> do
          sendMessage client (encodeMessage (req respSize payloadSize))
          _resp <- receiveMessage client
          -- TODO: decode response and verify length of response size field
          return ()
        sendHalfClose client
        Nothing <- receiveMessage client
        closeCall client
      case mds of
        RpcOk () ->
          return (Right ())
        RpcError err ->
          return (Left (show err))
