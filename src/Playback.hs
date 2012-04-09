-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
--
--  Copyright (C) 2010, 2011, 2012 Oleg Belozeorov
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

{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}

module Playback
  ( initPlayback
  , WithPlayback
  , withPlayback
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

import Data.Typeable
import Data.Env

import XMMS2.Client hiding (playbackStatus)
import qualified XMMS2.Client as XC

import XMMS
import Registry
import Utils


data Ix = Ix deriving (Typeable)

data Playback
  = Playback { _playbackStatus :: TVar (Maybe PlaybackStatus)
             , _currentTrack   :: TVar (Maybe (Int, String))
             }
    deriving (Typeable)

type WithPlayback = ?_Playback :: Playback

playbackStatus :: WithPlayback => TVar (Maybe PlaybackStatus)
playbackStatus = _playbackStatus ?_Playback

currentTrack :: WithPlayback => TVar (Maybe (Int, String))
currentTrack = _currentTrack ?_Playback

getPlaybackStatus :: WithPlayback => IO (Maybe PlaybackStatus)
getPlaybackStatus = atomically $ readTVar playbackStatus

getCurrentTrack :: WithPlayback => IO (Maybe (Int, String))
getCurrentTrack = atomically $ readTVar currentTrack

initPlayback :: WithRegistry => IO ()
initPlayback = withXMMS $ do
  playback <- mkPlayback
  addEnv Ix playback
  let ?_Playback = playback

  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    resetState
    when conn $ do
      broadcastPlaybackStatus xmms >>* do
        liftIO requestStatus
        return True
      requestStatus
      broadcastPlaylistCurrentPos xmms >>* do
        liftIO requestCurrentTrack
        return True
      requestCurrentTrack

  return ()

withPlayback :: WithRegistry => (WithPlayback => IO a) -> IO a
withPlayback func = do
  Just (Env playback) <- getEnv (Extract :: Extract Ix Playback)
  let ?_Playback = playback
  func

mkPlayback :: IO Playback
mkPlayback = do
  playbackStatus <- atomically $ newTVar Nothing
  currentTrack   <- atomically $ newTVar Nothing
  return Playback { _playbackStatus = playbackStatus
                  , _currentTrack   = currentTrack
                  }

resetState :: WithPlayback => IO ()
resetState = atomically $ do
  writeTVar playbackStatus Nothing
  writeTVar currentTrack Nothing

setStatus :: WithPlayback => Maybe PlaybackStatus -> IO ()
setStatus s = atomically $ writeTVar playbackStatus s

requestStatus :: (WithPlayback, WithXMMS) => IO ()
requestStatus =
  XC.playbackStatus xmms >>* do
    status <- result
    liftIO . setStatus $ Just status

requestCurrentTrack :: (WithPlayback, WithXMMS) => IO ()
requestCurrentTrack =
  playlistCurrentPos xmms Nothing >>* do
    new <- catchResult Nothing (Just . first fromIntegral)
    liftIO $ atomically $ writeTVar currentTrack new

startPlayback :: (WithPlayback, WithXMMS) => Bool -> IO ()
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

pausePlayback :: WithXMMS => IO ()
pausePlayback = do
  playbackPause xmms
  return ()

stopPlayback :: WithXMMS => IO ()
stopPlayback = do
  playbackStop xmms
  return ()

nextTrack :: WithXMMS => IO ()
nextTrack = do
  playlistSetNextRel xmms 1
  playbackTickle xmms
  return ()

prevTrack :: WithXMMS => IO ()
prevTrack = do
  playlistSetNextRel xmms (-1)
  playbackTickle xmms
  return ()

restartPlayback :: WithXMMS => IO ()
restartPlayback = do
  playbackTickle xmms
  return ()
