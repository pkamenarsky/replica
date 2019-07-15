{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent.Async       (Async, async, waitCatchSTM, race)
import           Control.Concurrent.STM         (TMVar, TQueue, TVar, STM, atomically, retry, check, throwSTM
                                                , newTVar, readTVar, writeTVar
                                                , newTMVar, newEmptyTMVar, tryPutTMVar, readTMVar, isEmptyTMVar
                                                , newTQueue, writeTQueue, readTQueue)
import           Control.Monad                  (join, forever)
import           Control.Applicative            ((<|>))
import           Control.Exception              (SomeException(SomeException),Exception, throwIO, evaluate, try)

import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as A

import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Builder         as TB
import           Data.Bool                      (bool)
import           Data.Void                      (Void, absurd)
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
  r <- try $ do
    s <- firstStep initial step
    case s of
      Nothing ->
        pure Nothing
      Just (_initialVdom, startContext') -> do
        -- `initialVdom` can be used for SSR(server-side rendering). We aren't using it yet,
        -- so its prefixed by underscore `_`.
        ctx <- startContext'
        attachContextToWebsocket conn ctx
  case r of
    Left (SomeException e)         -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right (Just (SomeException e)) -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right _                        -> sendClose conn ("done" :: T.Text)
  where
    closeCodeInternalError = 1011


-- 恐らく実装エラーにより発生したであろう
-- These exceptions are not to ment recoverable. Should stop the context.
-- TODO: more rich to make debuggin easier?
data ContextError
  = IllformedData
  | InvalidEvent
  deriving Show

instance Exception ContextError

data Frame = Frame
  { frameNumber :: Int
  , frameVdom :: V.HTML
  , frameFire :: Event -> Maybe (IO ())
  }

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
firstStep
  :: st
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> IO (Maybe (V.HTML, IO Context))
firstStep initial step = do
  r <- step initial
  case r of
    Nothing ->
      pure Nothing
    Just (_vdom, st, fire) -> do
      vdom <- evaluate _vdom
      pure $ Just (vdom, startContext step (vdom, st, fire))

-- | Attacehes context to webcoket connection and start
--
-- This function will block until:
--
--   1) Connection/Protocol-wise exception thrown
--   2) Context ends gracefully, returning `Nothing`
--   3) Context ends by exception, returning `Just SomeException`
--
-- Some notes:
--
--   * Assumes this context is not attached to any other connection.
--   * Connection/Protocol-wise exception(e.g. connection closed by client) will not stop the context.
--   * Atleast one frame will always be sent immiedatly. Even in a case where context is already
--     over/stopped by exception. In those case, it sends one frame and immiedeatly returns.
--   * First frame will be sent as `ReplaceDOM`, following frame will be sent as `UpdateDOM`
--   * In some rare case, when stepLoop is looping too fast, few frame might be get skipped,
--     but its not much a problems since those frame would have been shown only for a moment.
--
-- Yeah, this framework is probably not meant for showing smooth animation.
-- We can actually mitigate this by preserving recent frames, not just the latest one.
--
attachContextToWebsocket :: Connection -> Context -> IO (Maybe SomeException)
attachContextToWebsocket conn ctx = withWorker eventLoop frameLoop
  where
    frameLoop :: IO (Maybe SomeException)
    frameLoop = do
      v@(f, _) <- atomically $ readTVar (ctxFrame ctx)
      sendTextData conn $ A.encode $ ReplaceDOM (frameVdom f)
      frameLoop' v

    frameLoop' :: (Frame, TMVar (Maybe Event)) -> IO (Maybe SomeException)
    frameLoop' (prevFrame, prevStepedBy) = do
      e <- atomically $  Left <$> getNewerFrame <|> Right <$> waitCatchSTM (ctxThread ctx)
      case e of
        Left (v@(frame,_), stepedBy) -> do
          diff <- evaluate $ V.diff (frameVdom frame) (frameVdom prevFrame)
          let updateDom = UpdateDOM (frameNumber frame) (evtClientFrame <$> stepedBy) diff
          sendTextData conn $ A.encode $ updateDom
          frameLoop' v
        Right result ->
          pure $ either Just (const Nothing) result
      where
        getNewerFrame = do
          v@(f, _) <- readTVar (ctxFrame ctx)
          check $ frameNumber f > frameNumber prevFrame
          s <- readTMVar prevStepedBy  -- This should not block if we implement propertly. See `Context`'s documenation.
          pure (v, s)

    eventLoop :: IO Void
    eventLoop = forever $ do
      ev' <- A.decode <$> receiveData conn
      ev  <- maybe (throwIO IllformedData) pure ev'
      atomically $ writeTQueue (ctxEventQueue ctx) ev

----------------------------------------------------------------------------

-- | Context
--
--  * For every frame, its corresponding TMVar should get a value before the next (frame,stepedBy) is written.
--  * Only exception to this is when exception occurs, last setted frame's `stepedBy` could be empty forever.
--
-- TODO: TMVar in a TVar. Is that a good idea?
-- TODO: name `Context` doesn't sound good.. rename
data Context = Context
  { ctxFrame      :: TVar (Frame, TMVar (Maybe Event))
  , ctxEventQueue :: TQueue Event   -- TBqueue might be better
  , ctxThread     :: Async ()
  }

startContext
  :: (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> (V.HTML, st, Event -> Maybe (IO ()))
  -> IO Context
startContext step (vdom, st, fire) = do
  let frame0 = Frame 0 vdom (const $ Just $ pure ())
  let frame1 = Frame 1 vdom fire
  (fv, qv) <- atomically $ do
    r <- newTMVar Nothing
    f <- newTVar (frame0, r)
    q <- newTQueue
    pure (f, q)
  th <- async $ withWorker
    (fireLoop (getNewFrame fv) (getEvent qv))
    (stepLoop (setNewFrame fv) step st frame1)
  pure $ Context fv qv th
  where
    setNewFrame var f = atomically $ do
      r <- newEmptyTMVar
      writeTVar var (f,r)
      pure r

    getNewFrame var = do
      v@(_, r) <- readTVar var
      bool retry (pure v) =<< isEmptyTMVar r

    getEvent que = readTQueue que

stepLoop
  :: (Frame -> IO (TMVar (Maybe Event)))
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> st
  -> Frame
  -> IO ()
stepLoop setNewFrame step st frame = do
  stepedBy <- setNewFrame frame
  r <- step st
  _ <- atomically $ tryPutTMVar stepedBy Nothing
  case r of
    Nothing -> pure ()
    Just (_newVdom, newSt, newFire) -> do
      newVdom <- evaluate _newVdom
      let newFrame = Frame (frameNumber frame + 1) newVdom newFire
      stepLoop setNewFrame step newSt newFrame

-- (1) STM's (<|>) は左偏があることに注意。
--   どちらを左にしても event lost は防げない...
-- (2) 上記の (<|>) の左偏により stepedBy は既に入っている可能性がある
fireLoop
  :: STM (Frame, TMVar (Maybe Event))
  -> STM Event
  -> IO Void
fireLoop getNewFrame getEvent = forever $ do
  (frame, stepedBy) <- atomically getNewFrame
  let act = atomically $ do
        r <- Left <$> getEvent <|> Right <$> readTMVar stepedBy -- (1)
        case r of
          Left ev -> case frameFire frame ev of
            Nothing
              | evtClientFrame ev < frameNumber frame -> pure $ join act
              | otherwise -> throwSTM InvalidEvent
            Just fire' -> bool (pure ()) fire' <$> tryPutTMVar stepedBy (Just ev)   -- (2)
          Right _ -> pure $ pure ()
  join act

-- | Runs a worker action alongside the provided continuation.
-- The worker will be automatically torn down when the continuation
-- terminates.
withWorker
  :: IO Void -- ^ Worker to run
  -> IO a
  -> IO a
withWorker worker cont =
  either absurd id <$> race worker cont
