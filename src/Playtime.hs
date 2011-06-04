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

module Playtime
  ( initPlaytime
  , makeSeekControl
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as Map

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client hiding (playbackStatus)

import XMMS
import Handler
import Medialib
import Context
import Utils
import Playback


data Playtime
  = Playtime { pAdj :: Adjustment }

adj = pAdj context

data Msg
  = PT Int
  | CI (Maybe MediaId)
  | MI (MediaId, Stamp, MediaInfo)

initPlaytime = do
  context <- initContext
  let ?context = context

  ciV <- atomically $ newTVar Nothing
  ciW <- atomically $ newTWatch ciV Nothing
  ptV <- atomically $ newTVar 0
  ptW <- atomically $ newEmptyTWatch ptV
  miC <- atomically $ dupTChan mediaInfoChan
  onServerConnectionAdd . ever $ \conn ->
    if conn
    then do
      playbackCurrentId xmms >>* handleCurrentId ciV
      broadcastPlaybackCurrentId xmms >>* (handleCurrentId ciV >> persist)
      playbackPlaytime xmms >>* handlePlaytime ptV
      signalPlaybackPlaytime xmms >>* (handlePlaytime ptV >> persist)
    else do
      return ()

  forkIO $ forever $ do
    msg <- atomically $ msum [CI <$> watch ciW, PT <$> watch ptW, MI <$> readTChan miC]
    case msg of
      PT time -> adjustmentSetValue adj $ fromIntegral time
      CI mmid -> do
        adjustmentSetValue adj 0
        adjustmentSetUpper adj bigNum
        withJust mmid requestInfo
      MI (mi, _, info) -> do
        ci <- readTVarIO ciV
        when (ci == Just mi) $
          case Map.lookup "duration" info of
            Just (PropInt32 d) -> adjustmentSetUpper adj $ fromIntegral d
            _                  -> return ()

  return ?context

handlePlaytime ptV = do
  pt <- catchResult 0 fromIntegral
  liftIO $ atomically $ writeTVar ptV pt

handleCurrentId ciV = do
  ci <- catchResult Nothing Just
  liftIO $ atomically $ writeTVar ciV ci


initContext = do
  adj <- adjustmentNew 0 0 bigNum 5000 5000 0
  return $ augmentContext
    Playtime { pAdj = adj }

bigNum = 10000000000.0

makeSeekControl = do
  view <- hScaleNew adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  vw <- atomically newEmptyTMVar
  view `onDestroy` (atomically $ putTMVar vw ())
  sw <- atomically $ newEmptyTWatch playbackStatus
  let mgr = do
        msg <- atomically $ msum [Left <$> takeTMVar vw, Right <$> watch sw]
        case msg of
          Left _  -> return ()
          Right s -> do
            widgetSetSensitive view $ s == (Just StatusPlay)
            mgr
  forkIO mgr

  return view
