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
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Exception
import           Control.Monad
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.Mem

import qualified Data.ByteString                       as B
import qualified Data.ByteString.Char8                 as C8
import qualified Data.ByteString.Lazy                  as L

import           Network.Grpc.Core.Call
import           Network.Grpc.Lib.Grpc
import           Network.Grpc.Lib.TimeSpec
import           Network.Grpc.Lib.Types

import           Data.Default.Class                    (def)
import           Data.ProtoLens                        (decodeMessage,
                                                        encodeMessage)
import           Proto.Src.Proto.Grpc.Testing.Messages

data Options = Options
  { optServerHost            :: String
  , optServerHostOverride    :: String
  , optServerPort            :: Int
  , optTestCase              :: TestCase
  , optUseTLS                :: Bool
  , optUseTestCA             :: Bool
  , optDefaultServiceAccount :: String
  , optOAuthScope            :: String
  , optServiceAccountKeyFile :: FilePath
}

data TestCase
  = EmptyUnary
  | LargeUnary
  | AllTests
  | TestCaseUnknown String
  deriving Show

allTests :: [TestCase]
allTests =
  [ EmptyUnary
  , LargeUnary
  ]

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

testCase :: String -> TestCase
testCase "empty_unary" = EmptyUnary
testCase "large_unary" = LargeUnary
testCase "all"         = AllTests
testCase unknown       = TestCaseUnknown unknown

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
      "The name of the test case to execute. For example, \"empty_unary\""
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

testOptions :: [String] -> Either [String] Options
testOptions flags =
  case getOpt Permute options flags of
    (opts, [], []) -> Right (foldl (flip id) defaultOptions opts)
    (_, unkn, err) -> Left (map ("unrecognized argument " ++) unkn ++ err)

main :: IO ()
main = do
  args <- getArgs
  opts <- case testOptions args of
    Left errs -> do
      putStrLn "Errors while parsing flags:"
      mapM_ putStrLn errs
      putStrLn (usageInfo "Usage: interop_client [OPTION]" options)
      exitFailure
    Right opts -> return opts
  case optTestCase opts of
    AllTests ->
      forM_ allTests $ \tc -> do
        let opts' = opts { optTestCase = tc }
        testWrapper tc (runTest tc opts')
    tc ->
      testWrapper tc (runTest tc opts)

testWrapper :: TestCase -> IO (Either String ()) -> IO ()
testWrapper tc act =
  bracket_
    grpcInit
    (performMajorGC >> grpcShutdown)
    (do
      result <- act
      case result of
        Right _ -> putStrLn (show tc ++ ": ok")
        Left err -> do
          putStrLn err
          exitFailure)

runTest :: TestCase -> Options -> IO (Either String ())
runTest EmptyUnary opts = runEmptyUnaryTest opts
runTest LargeUnary opts = runLargeUnaryTest opts
runTest (TestCaseUnknown tc) _ =
  return (Left ("Unknown test case, or not specified: " ++ tc))

hostPort :: Options -> B.ByteString
hostPort Options{..} = C8.pack (optServerHost ++ ":" ++ show optServerPort)

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
  bracket (grpcInsecureChannelCreate (hostPort opts) emptyChannelArgs reservedPtr) grpcChannelDestroy $ \channel -> do
    deadline <- secondsFromNow 1
    bracket (fmap (withTimeout deadline) (newClientContext channel)) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx "/grpc.testing.TestService/EmptyCall" B.empty []
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
  bracket (grpcInsecureChannelCreate (hostPort opts) emptyChannelArgs reservedPtr) grpcChannelDestroy $ \channel -> do
    deadline <- secondsFromNow 1
    bracket (fmap (withTimeout deadline) (newClientContext channel)) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx "/grpc.testing.TestService/UnaryCall" (encodeMessage req) []
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