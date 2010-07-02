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

{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Concurrent.MVar
import Control.Monad.Trans

import XMMS2.Client

import XMMS
import Handler
import Env
import Utils


data State
  = State { sCurrentTrack :: Maybe (Int, String)
          , sStatus       :: Maybe PlaybackStatus
          }

data Playback
  = Playback { pState            :: MVar State
             , pOnPlaybackStatus :: HandlerMVar (Maybe PlaybackStatus)
             , pOnCurrentTrack   :: HandlerMVar (Maybe (Int, String))
             }

state = pState getEnv

onPlaybackStatus = onHandler (pOnPlaybackStatus getEnv)
onPlaybackStatusAdd f = do
  id <- onPlaybackStatus . add $ f
  f =<< getPlaybackStatus
  return id

onCurrentTrack = onHandler (pOnCurrentTrack getEnv)

getPlaybackStatus =
  withMVar state $ return . sStatus

getCurrentTrack =
  withMVar state $ return . sCurrentTrack


initPlayback = do
  env <- initEnv
  let ?env = env

  onServerConnectionAdd . ever $ \conn ->
    if conn
    then do
      broadcastPlaybackStatus xmms >>* do
        liftIO requestStatus
        return True
      requestStatus
      broadcastPlaylistCurrentPos xmms >>* do
        liftIO $ requestCurrentTrack
        return True
      requestCurrentTrack
    else
      resetState

  return ?env


initEnv = do
  state            <- newMVar makeState
  onPlaybackStatus <- makeHandlerMVar
  onCurrentTrack   <- makeHandlerMVar
  return $ augmentEnv
    Playback { pState            = state
             , pOnPlaybackStatus = onPlaybackStatus
             , pOnCurrentTrack   = onCurrentTrack
             }

makeState =
  State { sCurrentTrack = Nothing
        , sStatus       = Nothing
        }

resetState = do
  modifyMVar_ state $ const $ return makeState
  invokeOnPlaybackStatus

setStatus s = do
  modifyMVar_ state $ \state ->
    return state { sStatus = s }
  invokeOnPlaybackStatus

requestStatus =
  playbackStatus xmms >>* do
    status <- result
    liftIO . setStatus $ Just status
    return False

requestCurrentTrack =
  playlistCurrentPos xmms Nothing >>* do
    new <- catchResult Nothing (Just . mapFst fromIntegral)
    liftIO $ do
      old <- modifyMVar state $ \state ->
        return (state { sCurrentTrack = new }, sCurrentTrack state)
      onCurrentTrack $ invoke old
    return False

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
