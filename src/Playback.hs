-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

module Playback
  ( initPlayback
  , onPlaybackStatus
  , onPlaybackStatusAdd
  , onCurrentTrack
  , getPlaybackStatus
  , getCurrentTrack
  , startPlayback
  , pausePlayback
  , stopPlayback
  , restartPlayback
  , prevTrack
  , nextTrack
  , requestCurrentTrack
  ) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Trans

import XMMS2.Client hiding (playbackStatus)
import qualified XMMS2.Client as XC

import XMMS
import Handler
import Context
import Utils


data Playback
  = Playback { pPlaybackStatus   :: TVar (Maybe PlaybackStatus)
             , pOnPlaybackStatus :: HandlerMVar (Maybe PlaybackStatus)
             , pCurrentTrack     :: TVar (Maybe (Int, String))
             , pOnCurrentTrack   :: HandlerMVar (Maybe (Int, String))
             }

playbackStatus = pPlaybackStatus context
currentTrack   = pCurrentTrack context

onPlaybackStatus = onHandler (pOnPlaybackStatus context)
onPlaybackStatusAdd f = do
  id <- onPlaybackStatus . add $ f
  f =<< getPlaybackStatus
  return id

onCurrentTrack = onHandler (pOnCurrentTrack context)

getPlaybackStatus =
  atomically $ readTVar playbackStatus

getCurrentTrack =
  atomically $ readTVar currentTrack


initPlayback = do
  context <- initContext
  let ?context = context

  onServerConnectionAdd . ever $ \conn ->
    if conn
    then do
      broadcastPlaybackStatus xmms >>* do
        liftIO requestStatus
        persist
      requestStatus
      broadcastPlaylistCurrentPos xmms >>* do
        liftIO requestCurrentTrack
        persist
      requestCurrentTrack
    else
      resetState

  return ?context


initContext = do
  playbackStatus   <- atomically $ newTVar Nothing
  onPlaybackStatus <- makeHandlerMVar
  currentTrack     <- atomically $ newTVar Nothing
  onCurrentTrack   <- makeHandlerMVar
  return $ augmentContext
    Playback { pPlaybackStatus   = playbackStatus
             , pOnPlaybackStatus = onPlaybackStatus
             , pCurrentTrack     = currentTrack
             , pOnCurrentTrack   = onCurrentTrack
             }

resetState = do
  atomically $ do
    writeTVar playbackStatus Nothing
    writeTVar currentTrack Nothing
  invokeOnPlaybackStatus

setStatus s = do
  atomically $ writeTVar playbackStatus s
  invokeOnPlaybackStatus

requestStatus =
  XC.playbackStatus xmms >>* do
    status <- result
    liftIO . setStatus $ Just status

requestCurrentTrack =
  playlistCurrentPos xmms Nothing >>* do
    new <- catchResult Nothing (Just . first fromIntegral)
    liftIO $ do
      old <- atomically $ do
        old <- readTVar currentTrack
        writeTVar currentTrack new
        return old
      onCurrentTrack $ invoke old

startPlayback False = do
  playbackStart xmms
  return ()
startPlayback True = do
  ps <- getPlaybackStatus
  case ps of
    Just StatusPlay ->
      playbackTickle xmms
    Just StatusPause -> do
      playbackTickle xmms
      playbackStart xmms
      playbackTickle xmms
    _ ->
      playbackStart xmms
  return ()

pausePlayback = do
  playbackPause xmms
  return ()

stopPlayback = do
  playbackStop xmms
  return ()

nextTrack = do
  playlistSetNextRel xmms 1
  playbackTickle xmms
  return ()

prevTrack = do
  playlistSetNextRel xmms (-1)
  playbackTickle xmms
  return ()

invokeOnPlaybackStatus =
  onPlaybackStatus . invoke =<< getPlaybackStatus

restartPlayback = do
  playbackTickle xmms
  return ()
