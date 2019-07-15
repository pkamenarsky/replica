{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Replica where

import           Control.Concurrent.Async       (withAsync, link)
import           Control.Concurrent.STM
import           Control.Monad                  (join, void, forever)
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
import           Data.Void                      (Void)
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
        pure ()
      Just (_initialFrame, runner) ->
        -- `initialFrame` can be used for SSR(server-side rendering). We aren't using it yet,
        -- so its prefixed by underscore `_`.
        runner conn
  case r of
    Left (SomeException e) -> sendCloseCode conn closeCodeInternalError (T.pack $ show e)
    Right _                -> sendClose conn ("done" :: T.Text)
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
  -> IO (Maybe (Frame, Connection -> IO ()))
firstStep initial step = do
  r <- step initial
  case r of
    Nothing ->
      pure Nothing
    Just (_vdom, st, fire) -> do
      vdom <- evaluate _vdom
      let frame = Frame 1 vdom fire
      pure $ Just (frame, \conn -> running conn step (ReplaceDOM vdom) st frame)

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
running
  :: Connection
  -> (st -> IO (Maybe (V.HTML, st, Event -> Maybe (IO ()))))
  -> Update
  -> st
  -> Frame
  -> IO ()
running conn step update st frame = do

  sendTextData conn $ A.encode $ update                 -- (1)

  firedEvVar <- newIORef Nothing
  let readAndFireEvent = do                             -- (2.1)
        ev' <- A.decode <$> receiveData conn
        ev  <- maybe (throwIO IllformedData) pure ev'
        case frameFire frame ev of
          Nothing
            | evtClientFrame ev < frameNumber frame -> readAndFireEvent   --skip
            | otherwise -> throwIO InvalidEvent
          Just fire' -> do
            writeIORef firedEvVar (Just ev)
            fire'

  r <- withAsync' readAndFireEvent $ step st             -- (2.2)

  whenJust_ r $ \(_newVdom, newSt, newFire) -> do         -- (3)
    newVdom <- evaluate _newVdom
    diff    <- evaluate $ V.diff newVdom (frameVdom frame)
    firedEv <- readIORef firedEvVar
    let newUpdate = UpdateDOM (frameNumber frame + 1) (evtClientFrame <$> firedEv) diff
    let newFrame = Frame (frameNumber frame + 1) newVdom newFire
    running conn step newUpdate newSt newFrame

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

-- Like `withAsync`, but it also stops the second action when
-- first actions ends with exception.
withAsync' :: IO () -> IO a -> IO a
withAsync' w m = withAsync w (\a -> link a *> m)

whenJust_ :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust_ m f = void $ traverse f m
