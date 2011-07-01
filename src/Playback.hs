-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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
  , currentTrack
  , getPlaybackStatus
  , playbackStatus
  , getCurrentTrack
  , startPlayback
  , pausePlayback
  , stopPlayback
  , restartPlayback
  , prevTrack
  , nextTrack
  , requestCurrentTrack
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Arrow
import Control.Monad
import Control.Monad.Trans

import XMMS2.Client hiding (playbackStatus)
import qualified XMMS2.Client as XC

import XMMS
import Context
import Utils


data Playback
  = Playback { pPlaybackStatus   :: TVar (Maybe PlaybackStatus)
             , pCurrentTrack     :: TVar (Maybe (Int, String))
             }

playbackStatus = pPlaybackStatus context
currentTrack   = pCurrentTrack context

getPlaybackStatus =
  atomically $ readTVar playbackStatus

getCurrentTrack =
  atomically $ readTVar currentTrack


initPlayback = do
  context <- initContext
  let ?context = context

  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    resetState
    when conn $ do
      broadcastPlaybackStatus xmms >>* do
        liftIO requestStatus
        persist
      requestStatus
      broadcastPlaylistCurrentPos xmms >>* do
        liftIO requestCurrentTrack
        persist
      requestCurrentTrack

  return ?context


initContext = do
  playbackStatus   <- atomically $ newTVar Nothing
  currentTrack     <- atomically $ newTVar Nothing
  return $ augmentContext
    Playback { pPlaybackStatus   = playbackStatus
             , pCurrentTrack     = currentTrack
             }

resetState = atomically $ do
  writeTVar playbackStatus Nothing
  writeTVar currentTrack Nothing

setStatus s = atomically $
  writeTVar playbackStatus s

requestStatus =
  XC.playbackStatus xmms >>* do
    status <- result
    liftIO . setStatus $ Just status

requestCurrentTrack =
  playlistCurrentPos xmms Nothing >>* do
    new <- catchResult Nothing (Just . first fromIntegral)
    liftIO $ atomically $ writeTVar currentTrack new

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

restartPlayback = do
  playbackTickle xmms
  return ()
