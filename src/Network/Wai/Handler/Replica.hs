{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent             (forkIO, killThread)
import           Control.Concurrent.STM         (TVar, atomically, newTVarIO, readTVar, writeTVar, retry)
import           Control.Concurrent.Async       (withAsync, link)
import           Control.Monad                  (join, forever, void)
import           Control.Exception              (Exception, throwIO)

import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as A

import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Builder         as TB
import           Data.IORef                     (newIORef, readIORef, writeIORef)
import           Network.HTTP.Types             (status200)

import           Network.WebSockets             (ServerApp)
import           Network.WebSockets.Connection  (ConnectionOptions, Connection, acceptRequest, forkPingThread, receiveData, sendTextData)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)

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

data RunnerException
  = IllfomedData
  deriving Show

instance Exception RunnerException

app :: forall st.
     V.HTML
  -> ConnectionOptions
  -> st
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> Application
app index options initial step
  = websocketsOr options (websocketApp initial step) backupApp
  where
    indexBS = BL.fromStrict $ TE.encodeUtf8 $ TL.toStrict $ TB.toLazyText $ R.renderHTML index

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status200 [("content-type", "text/html")] indexBS

websocketApp :: forall st.
     st
  -> (st -> IO (Maybe (V.HTML, st, Event -> IO ())))
  -> ServerApp
websocketApp initial step pendingConn = do
  conn <- acceptRequest pendingConn
  forkPingThread conn 30
  run conn
  where
    -- More clear version
    -- change
    --
    --    * We should distinguish *frame* and *frame ID*.
    --    * Excluding the *first step* from loop. We'll need to do this for SSR(server-side rendering)
    --    * Two thread commnicating throw two tvar, its kinad hard to reason about and making
    --      it hard to understand the flow.
    --      We only need concurency while `running` function (2) part, so changed only that
    --      part runs concurrently.
    --    * changed the loop to make showing the vdom first. I think this is more easy to reason.
    --
    -- minor changes
    --
    --    * whenJust
    --    *
    --
    run :: Connection -> IO ()
    run conn = do
      -- 0. Get initial Frame.
      --
      -- st/step
      r <- step initial
      whenJust_ r $ \(vdom, st, fire) -> running conn (ReplaceDOM vdom) vdom st fire 1

    -- | Running loop
    --
    -- 1) Show frame(#id = frameId) to client
    --
    -- 2) 次の一歩を進める。
    -- This is the most hard part.
    --
    -- 3). If its not over yet, prepare for the next step.
    --
    --  * clientFrameId means, ...<TODO>
    --
    -- TODO: name `update` doesn't fit..
    -- TODO: Frame { frameId :: Int, html :: V.HTML, fire :: (Event -> IO ()) } が正しい気がしてきた。
    running :: Connection -> Update -> V.HTML -> st -> (Event -> IO ()) -> Int -> IO ()
    running conn update vdom st fire frameId = do

      sendTextData conn $ A.encode $ update                 -- (1)

      firedEvVar <- newIORef Nothing
      let readAndFireEvent = do                             -- (2.1)
            ev' <- A.decode <$> receiveData conn
            ev  <- maybe (throwIO IllfomedData) pure ev'
            writeIORef firedEvVar (Just ev)
            fire ev

      r <- withAsync' readAndFireEvent $ step st             -- (2.2)

      whenJust_ r $ \(newVdom, newSt, newFire) -> do         -- (3)
        firedEvMaybe <- readIORef firedEvVar
        let newUpdate = UpdateDOM
              frameId
              (evtClientFrame <$> firedEvMaybe)
              (V.diff newVdom vdom)
        running conn newUpdate newVdom newSt newFire (frameId + 1)

    -- Like `withAsync`, but it also stops the second action when
    -- first actions ends with exception.
    withAsync' :: IO () -> IO a -> IO a
    withAsync' w m = withAsync w (\a -> link a *> m)

    whenJust_ :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenJust_ m f = void $ traverse f m
