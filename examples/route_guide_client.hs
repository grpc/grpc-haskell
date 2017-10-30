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
import           Network.Grpc.Lib.ByteBuffer
import           Network.Grpc.Lib.Core
import           Network.Grpc.Lib.Metadata
import           Network.Grpc.Lib.Version

getFeatureMethodName :: Slice
getFeatureMethodName = "/routeguide.RouteGuide/GetFeature"

listFeaturesMethodName :: Slice
listFeaturesMethodName = "/routeguide.RouteGuide/ListFeatures"

recordRouteMethodName :: Slice
recordRouteMethodName = "/routeguide.RouteGuide/RecordRoute"

routeChatMethodName :: Slice
routeChatMethodName = "/routeguide.RouteGuide/RouteChat"

main :: IO ()
main = withGrpc main'

main' :: IO ()
main' = do
  BC8.putStrLn version
  channel <- createInsecureChannel "localhost" 10001 mempty
  ctx <- newClientContext channel

  let stub = createRouteGuideStub channel ctx

  measure "getFeature 1" $ do
    res <- runRpc (getFeature stub (Point 2 2))
    print res

  measure "getFeature 2" $ do
    let stub' = stub `withCallOptions` withMetadata [Metadata "my-key" "my-value" 0]
    res <- runRpc (getFeature stub' (Point 42 42))
    print res

  measure "listFeatures" $ do
    let rect = Rectangle (Point 0 0) (Point 16 16)
    features <- runRpc $ do
      reader <- listFeatures stub rect
      let
        readAll acc = do
          msg <- receiveMessage reader
          case msg of
            Just m -> do
              liftIO $ putStrLn ("got message: " ++ show m)
              readAll (m:acc)
            Nothing -> do
              liftIO $ putStrLn "No more messages"
              return (reverse acc)
      msgs <- readAll []
      status <- waitForStatus reader
      closeCall reader
      return (msgs, status)
    print features

  measure "recordRoute" $ do
    rt <- runRpc $ do
      record <- recordRoute stub
      forM_ [ Point x x | x <- [0..20] ] $ \p ->
        sendMessage record (fromPoint p)
      sendHalfClose record
      x <- receiveMessage record
      closeCall record
      return x
    print rt

  measure "async routeChat" $ do
    RpcOk msgs <- runRpc $ do
      route <- routeChat stub
      block <- liftIO newEmptyMVar
      _ <- liftIO $ forkIO $ do
        RpcOk msgs <- runRpc (receiveAllMessages route)
        putMVar block msgs
      mapM_ (sendMessage route) notes
      sendHalfClose route
      msgs <- liftIO (takeMVar block)
      closeCall route
      return msgs
    putStrLn ("got " ++ show (length msgs) ++ " messages")

  putStrLn "*** Destroying client context"
  destroyClientContext ctx

  putStrLn "*** Destroying channel"
  destroyChannel channel

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
      performMajorGC
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

callRpc :: IO (RpcReply a) -> Rpc a
callRpc io = liftIO io >>= joinReply

getFeature :: RouteGuideStub -> Point -> Rpc (UnaryResult L.ByteString)
getFeature client arg =
  callRpc (_getFeature client (_clientContext client) (_callOptions client) arg)

listFeatures :: RouteGuideStub -> Rectangle -> Rpc (Client B.ByteString L.ByteString)
listFeatures client arg =
  callRpc (_listFeatures client (_clientContext client) (_callOptions client) arg)

recordRoute :: RouteGuideStub -> Rpc (Client B.ByteString RouteSummary)
recordRoute client =
  callRpc (_recordRoute client (_clientContext client) (_callOptions client))

routeChat :: RouteGuideStub -> Rpc (Client B.ByteString L.ByteString)
routeChat client =
  callRpc (_routeChat client (_clientContext client) (_callOptions client))

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
