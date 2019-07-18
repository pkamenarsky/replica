{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent             (forkIO, killThread)
import           Control.Concurrent.STM         (TVar, atomically, newTVarIO, readTVar, writeTVar, retry)
import           Control.Monad                  (join, forever)
import           Control.Exception              (SomeException(SomeException), evaluate, try)

import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as A

import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Builder         as TB
import           Network.HTTP.Types             (status200)

import           Network.WebSockets             (ServerApp)
import           Network.WebSockets.Connection  (ConnectionOptions, Connection, acceptRequest, forkPingThread, receiveData, sendTextData, sendClose, sendCloseCode)
import           Network.Wai                    (Application, Middleware, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.Wai.Middleware.Static as MwS
import           Network.Wai.Middleware.Static ((>->))

import qualified Replica.VDOM                   as V
import qualified Replica.VDOM.Render            as R

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
     V.HTML
  -> ConnectionOptions
  -> Middleware
  -> st
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> Application
app index options middleware initial step
  = websocketsOr options (websocketApp initial step) (middleware backupApp)

  where
    indexBS = BL.fromStrict $ TE.encodeUtf8 $ TL.toStrict $ TB.toLazyText $ R.renderHTML index

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [("content-type", "text/html")] indexBS

websocketApp :: forall st.
     st
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> ServerApp
websocketApp initial step pendingConn = do
  conn <- acceptRequest pendingConn
  chan <- newTVarIO Nothing
  cf   <- newTVarIO Nothing

  forkPingThread conn 30

  tid <- forkIO $ forever $ do
    msg <- receiveData conn
    case A.decode msg of
      Just msg' -> do
        join $ atomically $ do
          fire <- readTVar chan
          case fire of
            Just fire' -> do
              case fire' msg' of
                Just io -> do
                  writeTVar chan Nothing
                  writeTVar cf (Just $ evtClientFrame msg')
                  pure io
                Nothing -> pure (pure ())
            Nothing -> retry
      Nothing -> traceIO $ "Couldn't decode event: " <> show msg

  r <- try $ go conn chan cf Nothing initial 0

  case r of
    Left (SomeException e) -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right _ -> sendClose conn ("done" :: T.Text)

  killThread tid

  where
    closeCodeInternalError = 1011

    go :: Connection -> TVar (Maybe (Event -> Maybe (IO ()))) -> TVar (Maybe Int) -> Maybe V.HTML -> st -> Int -> IO ()
    go conn chan cf oldDom st serverFrame = do
      r <- step st
      case r of
        Nothing -> pure ()
        Just (newDom, next, fire) -> do
          clientFrame <- atomically $ do
            a <- readTVar cf
            writeTVar cf Nothing
            pure a

          -- Throw exceptions here
          newDom' <- evaluate newDom

          case oldDom of
            Nothing      -> sendTextData conn $ A.encode $ ReplaceDOM newDom'
            Just oldDom' -> do
              -- Throw exceptions here
              diff <- evaluate (V.diff oldDom' newDom')
              sendTextData conn $ A.encode $ UpdateDOM serverFrame clientFrame diff

          atomically $ writeTVar chan (Just fire)

          go conn chan cf (Just newDom) next (serverFrame + 1)

defaultMiddleware :: Middleware
defaultMiddleware = MwS.staticPolicy (MwS.noDots >-> MwS.addBase "static") 
