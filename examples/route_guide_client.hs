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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Time.Clock
import           Data.Word
import           Numeric
import           System.Mem

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC8
import qualified Data.ByteString.Lazy      as L

import           Network.Grpc.Core.Call
import           Network.Grpc.Lib.Core
import           Network.Grpc.Lib.Metadata
import           Network.Grpc.Lib.Version


getFeatureMethodName :: B.ByteString
getFeatureMethodName = "/routeguide.RouteGuide/GetFeature"

listFeaturesMethodName :: B.ByteString
listFeaturesMethodName = "/routeguide.RouteGuide/ListFeatures"

recordRouteMethodName :: B.ByteString
recordRouteMethodName = "/routeguide.RouteGuide/RecordRoute"

routeChatMethodName :: B.ByteString
routeChatMethodName = "/routeguide.RouteGuide/RouteChat"

main :: IO ()
main = withGrpc main'

main' :: IO ()
main' = do
  BC8.putStrLn version
  channel <- createInsecureChannel "localhost:10001" mempty
  ctx <- newClientContext channel

  let stub = createRouteGuideStub channel ctx

  measure "getFeature 1" $ do
    res <- getFeature stub (Point 2 2)
    print res

  measure "getFeature 2" $ do
    let stub' = stub `withCallOptions` withMetadata [Metadata "my-key" "my-value" 0]
    res <- getFeature stub' (Point 42 42)
    print res

  measure "listFeatures" $ do
    let rect = Rectangle (Point 0 0) (Point 16 16)
    reader <- listFeatures stub rect
    features' <- withNewClient reader $ do
      let
        readAll acc = do
          msg <- receiveMessage
          case msg of
            Just m -> do
              liftIO $ putStrLn ("got message: " ++ show m)
              readAll (m:acc)
            Nothing -> do
              liftIO $ putStrLn "No more messages"
              closeCall
              return (reverse acc)
      readAll []
    print features'

  measure "recordRoute" $ do
    record <- recordRoute stub
    rt <- withNewClient record $ do
      forM_ [ Point x x | x <- [0..20] ] $ \p ->
        sendMessage (fromPoint p)
      sendHalfClose
      x <- receiveMessage
      closeCall
      return x
      -- initMetadata' <- getInitialMetadata recvInitMetadata
      -- putStrLn ("got initial metadata: " ++ show initMetadata')
    print rt

  measure "async routeChat" $ do
    RpcOk route <- routeChat stub
    block <- newEmptyMVar
    _ <- forkIO $ do -- the go example does this in a concurrent thread
          --initMetadata <- getInitialMetadata recvInitMetadata
          --putStrLn ("got initial metadata: " ++ show initMetadata)
          RpcOk msgs <- withClient route receiveAllMessages
          putMVar block msgs
    RpcOk _ <- withClient route $ do
      mapM_ sendMessage notes
      sendHalfClose
    msgs <- takeMVar block
    RpcOk _ <- withClient route closeCall
    putStrLn ("got " ++ show (length msgs) ++ " messages")

  putStrLn "*** Destroying client context"
  destroyClientContext ctx

  putStrLn "*** Destroying channel"
  grpcChannelDestroy channel

-- ----------------------------------------------
-- Example data
-- ----------------------------------------------

-- | RouteNotes
notes :: [B.ByteString]
notes = map (B.pack . f)
  [ "0a021001120d4669727374206d657373616765"
  , "0a021002120e5365636f6e64206d657373616765"
  , "0a021003120d5468697264206d657373616765"
  , "0a021001120e466f75727468206d657373616765"
  , "0a021002120d4669667468206d657373616765"
  , "0a021003120d5369787468206d657373616765"
  ]
  where
    hex :: Char -> Char -> Word8
    hex x y = fst . head $ readHex [x,y]
    f (a:b:c) = hex a b : f c
    f _       = []

-- ----------------------------------------------
-- Utils
-- ----------------------------------------------

withGrpc :: IO a -> IO a
withGrpc = bracket_ grpcInit (performMajorGC >> grpcShutdown)

measure :: String -> IO a -> IO a
measure desc io = bracket aquire release (\_ -> io)
  where
    aquire = do
      putStrLn "###"
      putStrLn ("### " ++ desc)
      putStrLn "###"
      getCurrentTime
    release start = do
      end <- getCurrentTime
      putStrLn (" - timing: " ++ show (diffUTCTime end start))
      putStrLn ""
      putStrLn ""

-- -------------------------------------------------------
-- A proto library should generate the stub and data types
-- -------------------------------------------------------

data Rectangle = Rectangle Point Point

fromRectangle :: Rectangle -> B.ByteString
fromRectangle (Rectangle (Point a b) (Point c d)) = B.pack [0x0a, 0x04, 0x08, a, 0x10, b, 0x12, 0x04, 0x08, c, 0x10, d]

data Point = Point Word8 Word8

fromPoint :: Point -> B.ByteString
fromPoint (Point a b) = B.pack [0x08, a, 0x10, b]

type RouteSummary = L.ByteString

data RouteGuideStub = RouteGuideStub {
  _channel       :: Channel,
  _callOptions   :: CallOptions,
  _clientContext :: ClientContext,
  _getFeature    :: ClientContext -> CallOptions -> Point -> IO (RpcReply (UnaryResult L.ByteString)),
  _listFeatures  :: ClientContext -> CallOptions -> Rectangle -> IO (RpcReply (Client B.ByteString L.ByteString)),
  _recordRoute   :: ClientContext -> CallOptions -> IO (RpcReply (Client B.ByteString RouteSummary)),
  _routeChat     :: ClientContext -> CallOptions -> IO (RpcReply (Client B.ByteString L.ByteString))
}

withCallOptions :: RouteGuideStub -> CallOptions -> RouteGuideStub
withCallOptions client co = client { _callOptions = co }

getFeature :: RouteGuideStub -> Point -> IO (RpcReply (UnaryResult L.ByteString))
getFeature client =
  _getFeature client (_clientContext client) (_callOptions client)

listFeatures :: RouteGuideStub -> Rectangle -> IO (RpcReply (Client B.ByteString L.ByteString))
listFeatures client =
  _listFeatures client (_clientContext client) (_callOptions client)

recordRoute :: RouteGuideStub -> IO (RpcReply (Client B.ByteString RouteSummary))
recordRoute client =
  _recordRoute client (_clientContext client) (_callOptions client)

routeChat :: RouteGuideStub -> IO (RpcReply (Client B.ByteString L.ByteString))
routeChat client =
  _routeChat client (_clientContext client) (_callOptions client)

createRouteGuideStub :: Channel -> ClientContext -> RouteGuideStub
createRouteGuideStub chan ctx0 = RouteGuideStub {
  _channel = chan,
  _callOptions = mempty,
  _clientContext = ctx0,
  _getFeature = \ctx co arg ->
    callUnary ctx co getFeatureMethodName (fromPoint arg),
  _listFeatures = \ctx co arg ->
    callDownstream ctx co listFeaturesMethodName (fromRectangle arg),
  _recordRoute = \ctx co ->
    callUpstream ctx co recordRouteMethodName,
  _routeChat = \ctx co ->
    callBidi ctx co routeChatMethodName
}
