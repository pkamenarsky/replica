{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent             (forkIO, killThread)
import           Control.Concurrent.STM         (TVar, atomically, newTVarIO, readTVar, writeTVar, retry)
import           Control.Monad                  (join, forever)

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
  | UpdateDOM Int (Maybe Int) [V.Diff]

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
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> Application
app title options initial step
  = websocketsOr options (websocketApp initial step) backupApp
  where
    indexBS = BL.fromStrict $ V.index title

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [] indexBS

websocketApp :: forall st.
     st
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> ServerApp
websocketApp initial step pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newTVarIO Nothing
  cf   <- newTVarIO Nothing

  forkPingThread conn 30
    
  tid <- forkIO $ forever $ do
    msg  <- receiveData conn
    case A.decode msg of
      Just msg' -> do
        join $ atomically $ do
          fire <- readTVar chan
          case fire of
            Just fire' -> do
              writeTVar chan Nothing
              writeTVar cf (Just $ evtClientFrame msg')
              pure (fire' msg')
            Nothing -> retry
      Nothing -> traceIO $ "Couldn't decode event: " <> show msg

  go conn chan cf Nothing initial 0

  killThread tid

  where
    go :: Connection -> TVar (Maybe (Event -> IO ())) -> TVar (Maybe Int) -> Maybe V.HTML -> st -> Int -> IO ()
    go conn chan cf oldDom st serverFrame = do
      r <- step st
      case r of
        Nothing -> pure ()
        Just (newDom, next, fire) -> do
          clientFrame <- atomically $ do
            a <- readTVar cf
            writeTVar cf Nothing
            pure a

          case oldDom of
            Nothing      -> sendTextData conn $ A.encode $ ReplaceDOM newDom
            Just oldDom' -> sendTextData conn $ A.encode $ UpdateDOM serverFrame clientFrame (V.diff oldDom' newDom)
          
          atomically $ writeTVar chan (Just fire)
    
          go conn chan cf (Just newDom) next (serverFrame + 1)
