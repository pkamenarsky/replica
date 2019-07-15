{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent.Async       (withAsync, link)
import           Control.Monad                  (void)
import           Control.Exception              (SomeException(SomeException),Exception, throwIO, evaluate, try)

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
import           Network.WebSockets.Connection  (ConnectionOptions, Connection, acceptRequest, forkPingThread, receiveData, sendTextData, sendClose, sendCloseCode)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Replica.VDOM                   as V
import qualified Replica.VDOM.Render            as R

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

-- 恐らく実装エラーにより発生したであろう
-- These exceptions are not to ment recoverable. Should stop the context.
-- TODO: more rich to make debuggin easier?
data RunnerException
  = IllfomedData
  | InvalidEvent
  deriving Show

instance Exception RunnerException

app :: forall st.
     V.HTML
  -> ConnectionOptions
  -> st
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> Application
app index options initial step
  = websocketsOr options (websocketApp initial step) backupApp
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
  forkPingThread conn 30
  r <- try $ run conn
  case r of
    Left (SomeException e) -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right _                -> sendClose conn ("done" :: T.Text)
  where
    closeCodeInternalError = 1011
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
      whenJust_ r $ \(_vdom, st, fire) -> do
        vdom <- evaluate _vdom
        running conn (ReplaceDOM vdom) vdom st fire 1

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
    running :: Connection -> Update -> V.HTML -> st -> (Event -> Maybe (IO ())) -> Int -> IO ()
    running conn update vdom st fire frameId = do

      sendTextData conn $ A.encode $ update                 -- (1)

      firedEvVar <- newIORef Nothing
      let readAndFireEvent = do                             -- (2.1)
            ev' <- A.decode <$> receiveData conn
            ev  <- maybe (throwIO IllfomedData) pure ev'
            case fire ev of
              Nothing
                | evtClientFrame ev < frameId -> readAndFireEvent   --skip
                | otherwise -> throwIO InvalidEvent
              Just fire' -> do
                writeIORef firedEvVar (Just ev)
                fire'

      r <- withAsync' readAndFireEvent $ step st             -- (2.2)

      whenJust_ r $ \(_newVdom, newSt, newFire) -> do         -- (3)
        newVdom <- evaluate _newVdom
        diff    <- evaluate $ V.diff newVdom vdom
        firedEv <- readIORef firedEvVar
        let newUpdate = UpdateDOM frameId (evtClientFrame <$> firedEv) diff
        running conn newUpdate newVdom newSt newFire (frameId + 1)

    -- Like `withAsync`, but it also stops the second action when
    -- first actions ends with exception.
    withAsync' :: IO () -> IO a -> IO a
    withAsync' w m = withAsync w (\a -> link a *> m)

    whenJust_ :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenJust_ m f = void $ traverse f m
