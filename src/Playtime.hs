-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
--
--  Copyright (C) 2010 Oleg Belozeorov
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3 of
--  the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details.
--

{-# LANGUAGE ScopedTypeVariables #-}

module Playtime
  ( initPlaytime
  , makeSeekControl
  ) where

import Prelude hiding (catch)

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO

import qualified Data.Map as Map
import Data.IORef
import Data.Word

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client

import XMMS
import Handler
import Medialib
import Env
import Utils
import Playback


data State
  = State { sCurrentId :: Maybe MediaId }

data Playtime
  = Playtime { pState :: MVar State
             , pAdj   :: Adjustment
             , pCId   :: ConnectId Adjustment }

state = pState getEnv
cId   = pCId getEnv
adj   = pAdj getEnv

getCurrentId    = withMVar state $ return . sCurrentId
setCurrentId id = modifyMVar_ state $ \s -> return  s { sCurrentId = id }

initPlaytime = do
  updateRef <- initUpdate
  let ?updateRef = updateRef

  env <- initEnv
  let ?env = env

  onServerConnection . add . ever $ \conn ->
    if conn
    then do
      playbackCurrentId xmms >>* handleCurrentId False
      broadcastPlaybackCurrentId xmms >>* handleCurrentId True
      playbackPlaytime xmms >>* handlePlaytime 0 False
      signalPlaybackPlaytime xmms >>* handlePlaytime 1000 True
    else do
      resetState
      setValue 0
      resetUpdate (+ 1)

  onMediaInfo . add . ever $ handleInfo

  return ?env


initEnv = do
  state <- newMVar makeState
  adj <- adjustmentNew 0 0 bigNum 5000 5000 0
  cId <- adj `onValueChanged` do
    pos <- adjustmentGetValue adj
    seek pos

  return $ augmentEnv
    Playtime { pState = state
             , pAdj   = adj
             , pCId   = cId }

makeState =
  State { sCurrentId = Nothing }

resetState =
  modifyMVar_ state $ const $ return makeState

handleCurrentId ret = do
  cid <- result
  liftIO $ do
    setCurrentId $
      if cid == 0
      then Nothing
      else Just cid
    requestInfo cid
  return ret

handleInfo (id, _, info) = do
  cid <- getCurrentId
  when (cid == Just id) $ do
    case Map.lookup "duration" info of
      Just (PropInt32 d) -> setUpper $ fromIntegral d
      _                  -> return ()

bigNum = 10000000000.0

seek pos = do
  putStrLn $ "seek to " ++ show pos
  eid <- disableUpdate
  playbackSeekMs xmms (round pos) SeekSet >>* do
    liftIO $ scheduleEnableUpdate eid
    return False
  return ()

data Update
  = Update { uEnabled   :: Bool
           , uEnablerId :: Maybe HandlerId
           , uStamp     :: Word32 }

initUpdate =
  newIORef Update { uEnabled   = True
                  , uEnablerId = Nothing
                  , uStamp     = 0 }

resetUpdate f =
  modifyIORef ?updateRef $ \up ->
    up { uEnabled   = True
       , uEnablerId = Nothing
       , uStamp     = f $ uStamp up }

updateEnabled = uEnabled <$> readIORef ?updateRef

disableUpdate = do
  up <- readIORef ?updateRef
  maybe (return ()) timeoutRemove $ uEnablerId up
  let stamp = uStamp up + 1
  writeIORef ?updateRef Update { uEnabled   = False
                               , uEnablerId = Nothing
                               , uStamp     = stamp }
  return stamp

scheduleEnableUpdate stamp = do
  eid <- (flip timeoutAdd) 500 $ do
    stamp' <- uStamp <$> readIORef ?updateRef
    when (stamp == stamp') $ resetUpdate id
    return False
  modifyIORef ?updateRef $ \up -> up { uEnablerId = Just eid }

getValue = adjustmentGetValue adj
setValue = withoutSeek . adjustmentSetValue adj
setUpper = withoutSeek . adjustmentSetUpper adj

withoutSeek =
  bracket_ (signalBlock cId) (signalUnblock cId)

handlePlaytime diff ret = do
  en <- liftIO $ updateEnabled
  when en $ do
    oldPt <- liftIO $ getValue
    newPt <- fromIntegral <$> result
             `catch` \(_ :: XMMSException) -> return 0
    when (abs (newPt - oldPt) >= diff) $
      liftIO $ setValue newPt
  return ret

makeSeekControl = do
  view <- hScaleNew adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False
  widgetSetSensitive view False

  ci <- onServerConnection . add . ever $ widgetSetSensitive view
  pi <- onPlaybackStatus . add . ever . const $ do
    s <- getPlaybackStatus
    widgetSetSensitive view $ s == Just StatusPlay
  view `onDestroy` do
    onServerConnection $ remove ci
    onPlaybackStatus $ remove pi

  return view
