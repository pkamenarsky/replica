{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent             -- (Chan, forkIO, newChan, readChan, writeChan)
import           Control.Concurrent.STM
import           Control.Monad                  (forever, void)

import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as A

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import           Network.HTTP.Types             (status200)

import           Network.WebSockets             (ServerApp)
import           Network.WebSockets.Connection  (ConnectionOptions, Connection, acceptRequest, forkPingThread, receiveData, sendTextData)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Replica.VDOM                   as V

import           Debug.Trace                    (traceIO)

data Event = Event
  { evtType        :: T.Text
  , evtEvent       :: A.Value
  , evtPath        :: [Int]
  , evtClientFrame :: Int
  } deriving Show

instance A.FromJSON Event where
  parseJSON (A.Object o) = Event
    <$> o .: "eventType"
    <*> o .: "event"
    <*> o .: "path"
    <*> o .: "clientFrame"
  parseJSON _ = fail "Expected object"

data Update
  = ReplaceDOM V.HTML
  | UpdateDOM Int Int [V.Diff]

instance A.ToJSON Update where
  toJSON (ReplaceDOM dom) = A.object
    [ "type" .= V.t "replace"
    , "dom"  .= dom
    ]
  toJSON (UpdateDOM serverFrame clientFrame ddiff) = A.object
    [ "type" .= V.t "update"
    , "serverFrame" .= serverFrame
    , "clientFrame" .= clientFrame
    , "diff" .= ddiff
    ]

app :: forall st.
     B.ByteString
  -> ConnectionOptions
  -> st
  -> (st -> IO (Maybe (V.HTML, Event -> IO st)))
  -> Application
app title options initial step
  = websocketsOr options (websocketApp initial step) backupApp
  where
    indexBS = BL.fromStrict $ V.index title

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [] indexBS

websocketApp :: forall st.
     st
  -> (st -> IO (Maybe (V.HTML, Event -> IO st)))
  -> ServerApp
websocketApp initial step pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newChan

  forkPingThread conn 30
    
  _ <- forkIO $ forever $ do
    msg <- receiveData conn
    case A.decode msg of
      Just e  -> writeChan chan e
      Nothing -> traceIO $ "Couldn't decode event: " <> show msg

  void $ go conn chan Nothing initial 0 0

  where
    go :: Connection -> Chan Event -> Maybe V.HTML -> st -> Int -> Int -> IO ()
    go conn chan oldDom st serverFrame clientFrame = do
      r <- step st
      case r of
        Nothing -> pure ()
        Just (newDom, await) -> do
          case oldDom of
            Nothing -> sendTextData conn $ A.encode $ ReplaceDOM newDom
            Just oldDom' -> sendTextData conn $ A.encode $ UpdateDOM serverFrame clientFrame (V.diff oldDom' newDom)
          
          event <- readChan chan
          newSt <- await event
    
          go conn chan (Just newDom) newSt (serverFrame + 1) (evtClientFrame event)

app' :: forall st.
     B.ByteString
  -> ConnectionOptions
  -> st
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> Application
app' title options initial step
  = websocketsOr options (websocketApp' initial step) backupApp
  where
    indexBS = BL.fromStrict $ V.index title

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [] indexBS

websocketApp' :: forall st.
     st
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> ServerApp
websocketApp' initial step pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newTVarIO Nothing
  cf   <- newTVarIO 0

  forkPingThread conn 30
    
  _ <- forkIO $ forever $ do
    msg  <- receiveData conn
    fire <- atomically $ do
      fire <- readTVar chan
      case (fire, A.decode msg) of
        (Just fire', Just msg') -> do
          writeTVar chan Nothing
          writeTVar cf (evtClientFrame msg')
          pure $ Right (fire' msg')
        (_, Nothing) -> pure $ Left msg
        (Nothing, _) -> retry

    case fire of
      Right io -> io
      Left e   -> traceIO $ "Couldn't decode event: " <> show e

  void $ go conn chan cf Nothing initial 0

  where
    go :: Connection -> TVar (Maybe (Event -> IO ())) -> TVar Int -> Maybe V.HTML -> st -> Int -> IO ()
    go conn chan cf oldDom st serverFrame = do
      r <- step st
      case r of
        Nothing -> pure ()
        Just (newDom, next, fire) -> do
          clientFrame <- readTVarIO cf
          case oldDom of
            Nothing -> sendTextData conn $ A.encode $ ReplaceDOM newDom
            Just oldDom' -> sendTextData conn $ A.encode $ UpdateDOM serverFrame clientFrame (V.diff oldDom' newDom)
          
          atomically $ writeTVar chan (Just fire)
    
          go conn chan cf (Just newDom) next (serverFrame + 1)
