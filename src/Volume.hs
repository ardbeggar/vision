-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

module Volume
  ( initVolume
  , makeVolumeControl
  ) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import qualified Data.Map as Map

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client

import XMMS
import Utils
import Context


data Volume
  = Volume { vAdj :: Adjustment }

adj = vAdj context

initVolume = do
  context <- initContext
  let ?context = context

  cId <- adj `onValueChanged` do
    vol <- adjustmentGetValue adj
    setVolume $ round vol

  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    if conn
      then do
      playbackVolumeGet xmms >>* do
        handleVolume cId
        liftIO $ broadcastPlaybackVolumeChanged xmms >>* do
          handleVolume cId
          persist
      else withoutVolumeChange cId $ adjustmentSetValue adj 0

  return ?context


initContext = do
  adj <- adjustmentNew 0 0 100 5 5 0
  return $ augmentContext Volume { vAdj = adj }


makeVolumeControl = do
  view <- hScaleNew adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  xcW <- atomically $ newTGWatch connectedV
  tid <- forkIO $ forever $ do
    conn <- atomically $ watch xcW
    widgetSetSensitive view conn
  view `onDestroy` killThread tid

  return view


handleVolume cId = do
  vol <- catchResult 0 (maximum . Map.elems)
  liftIO $ withoutVolumeChange cId $
    adjustmentSetValue adj $ fromIntegral vol

setVolume vol =
  playbackVolumeGet xmms >>* do
    vols <- catchResult Map.empty id
    liftIO $ mapM_ (flip (playbackVolumeSet xmms) vol) $ Map.keys vols

withoutVolumeChange cId =
  bracket_ (signalBlock cId) (signalUnblock cId)
