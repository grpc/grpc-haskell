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
import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C8
import qualified Data.ByteString.Lazy      as L

import           Network.Grpc.Core.Call
import           Network.Grpc.Lib.Grpc
import           Network.Grpc.Lib.TimeSpec
import           Network.Grpc.Lib.Types

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
  | TestCaseUnknown String

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
  runTest (optTestCase opts) opts

runTest :: TestCase -> Options -> IO ()
runTest EmptyUnary opts = runEmptyUnaryTest opts

runTest (TestCaseUnknown tc) _ = do
  putStrLn ("Unknown test case, or not specified: " ++ tc)
  exitFailure

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
runEmptyUnaryTest :: Options -> IO ()
runEmptyUnaryTest opts = flip finally grpcShutdown $ do
  grpcInit
  bracket (grpcInsecureChannelCreate (hostPort opts) emptyChannelArgs reservedPtr) grpcChannelDestroy $ \channel -> do
    deadline <- secondsFromNow 1
    bracket (fmap (withTimeout deadline) (newClientContext channel)) destroyClientContext $ \ctx -> do
      resp <- callUnary ctx "/grpc.testing.TestService/EmptyCall" B.empty []
      case resp of
        RpcOk (UnaryResult _ _ msg)
          | L.null msg -> putStrLn "all good"
          | otherwise -> do
              putStrLn "Non zero reply, failure."
              exitFailure
        RpcError err -> do
          print err
          exitFailure
