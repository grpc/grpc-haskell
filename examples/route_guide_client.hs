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
-- import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Time.Clock
import           Data.Word
import           Foreign.Ptr
import           Numeric
import           System.Mem

import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC8
import qualified Data.ByteString.Lazy         as L

import           Network.Grpc.Core.Call
import           Network.Grpc.Lib.TimeSpec
import           Network.Grpc.Lib.Metadata
import           Network.Grpc.Lib.Version
import           Network.Grpc.Lib.Grpc
import           Network.Grpc.Lib.Types


getFeatureMethodName :: B.ByteString
getFeatureMethodName = "/routeguide.RouteGuide/GetFeature"

listFeaturesMethodName :: B.ByteString
listFeaturesMethodName = "/routeguide.RouteGuide/ListFeatures"

recordRouteMethodName :: B.ByteString
recordRouteMethodName = "/routeguide.RouteGuide/RecordRoute"

routeChatMethodName :: B.ByteString
routeChatMethodName = "/routeguide.RouteGuide/RouteChat"

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
    f _ = []

main :: IO ()
main = do
  grpcInit
  BC8.putStrLn version
  channel <- grpcInsecureChannelCreate "localhost:10000" (ChannelArgs nullPtr) reservedPtr
  deadline <- secondsFromNow 2
  ctx <- withTimeout deadline <$> newClientContext channel
  putStrLn "==================== getFeature1"
  print =<< measure "getFeature1" (getFeature createRouteGuideClient ctx (Point 2 2) [Metadata "my-metadata-key" "foo" 0])
  putStrLn "==================== PASSED"
  putStrLn "==================== getFeature2"
  print =<< measure "getFeature2" (getFeature createRouteGuideClient ctx (Point 42 42) [])
  putStrLn "==================== PASSED"
  putStrLn "==================== listFeatures"
  let rect = Rectangle (Point 0 0) (Point 16 16)
  RpcOk reader <- listFeatures createRouteGuideClient ctx rect
  features' <- withClient reader $ do
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
  putStrLn "==================== PASSED"

  putStrLn "==================== recordRoute"
  RpcOk record <- recordRoute createRouteGuideClient ctx
  rt <- withClient record $ do
    forM_ [ Point x x | x <- [0..20] ] $ \p -> do
      sendMessage (fromPoint p)
    sendClose
    x <- receiveMessage
    closeCall
    return x
    -- initMetadata' <- getInitialMetadata recvInitMetadata
    -- putStrLn ("got initial metadata: " ++ show initMetadata')
  print rt
  putStrLn "==================== PASSED"

  putStrLn "==================== async routeChat"
  RpcOk route <- routeChat createRouteGuideClient ctx
  block <- newEmptyMVar
  _ <- forkIO $ do -- the go example does this in a concurrent thread
        --initMetadata <- getInitialMetadata recvInitMetadata
        --putStrLn ("got initial metadata: " ++ show initMetadata)
        RpcOk msgs <- withClient route $ receiveAllMessages
        putMVar block msgs
  RpcOk _ <- withClient route $ do
    mapM_ sendMessage notes
  msgs <- takeMVar block
  RpcOk _ <- withClient route $ do
    sendClose
    closeCall
  putStrLn ("got " ++ show (length msgs) ++ " messages")
  putStrLn "==================== PASSED"

  putStrLn "*** Destroying channel"
  grpcChannelDestroy channel
  putStrLn "*** Shutting down client context"
  destroyClientContext ctx
  putStrLn "*** Doing major GC"
  performMajorGC
  putStrLn "*** grpc shutdown"
  grpcShutdown

unfoldM :: (Monad m) => m (Maybe a) -> m [a]
unfoldM act = go []
  where
    go acc = do
      x <- act
      case x of
        Nothing -> return (reverse acc)
        Just x' -> go (x':acc)

unfoldM' :: Show a => IO (Maybe a) -> IO [a]
unfoldM' act = go []
  where
    go acc = do
      x <- act
      case x of
        Nothing -> return (reverse acc)
        Just x' -> print x' >> go (x':acc)

measure :: String -> IO a -> IO a
measure desc io = bracket aquire release (\_ -> io)
  where
    aquire = getCurrentTime
    release start = do
      end <- getCurrentTime
      putStrLn (desc ++ ": " ++ show (diffUTCTime end start))

data Rectangle = Rectangle Point Point

fromRectangle :: Rectangle -> B.ByteString
fromRectangle (Rectangle (Point a b) (Point c d)) = B.pack [0x0a, 0x04, 0x08, a, 0x10, b, 0x12, 0x04, 0x08, c, 0x10, d]

data Point = Point Word8 Word8

fromPoint :: Point -> B.ByteString
fromPoint (Point a b) = B.pack [0x08, a, 0x10, b]

data Feature = Feature String Point

type RouteSummary = L.ByteString

data RouteGuideClient = RouteGuideClient {
  getFeature   :: ClientContext -> Point -> [Metadata] -> IO (RpcReply (UnaryResult L.ByteString)),
  listFeatures :: ClientContext -> Rectangle -> IO (RpcReply (Client B.ByteString L.ByteString)),
  recordRoute  :: ClientContext -> IO (RpcReply (Client B.ByteString RouteSummary)),
  routeChat    :: ClientContext -> IO (RpcReply (Client B.ByteString L.ByteString))
}

createRouteGuideClient :: RouteGuideClient
createRouteGuideClient = RouteGuideClient {
  getFeature = \ctx arg mds ->
    callUnary ctx getFeatureMethodName (fromPoint arg) mds,
  listFeatures = \ctx rect ->
    callDownstream ctx listFeaturesMethodName (fromRectangle rect),
  recordRoute = \ctx ->
    callUpstream ctx recordRouteMethodName,
  routeChat = \ctx ->
    callBidi ctx routeChatMethodName
}

