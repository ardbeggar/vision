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

module Volume
  ( initVolume
  , makeVolumeControl
  ) where

import Control.Monad.Trans

import qualified Data.Map as Map

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client

import XMMS
import Handler
import Utils
import Env


data Volume
  = Volume { vAdj :: Adjustment
           , vCid :: ConnectId Adjustment }

adj = vAdj getEnv
cId = vCid getEnv

initVolume = do
  env <- initEnv
  let ?env = env

  onServerConnectionAdd . ever $ \conn ->
    if conn
    then do
      playbackVolumeGet xmms >>* do
        handleVolume
        lift $ broadcastPlaybackVolumeChanged xmms >>* handleVolume
        return False
      signalUnblock cId
    else do
      signalBlock cId
      withoutVolumeChange $ adjustmentSetValue (adj) 0

  return ?env


initEnv = do
  adj <- adjustmentNew 0 0 100 5 5 0
  cId <- adj `onValueChanged` do
    vol <- adjustmentGetValue adj
    setVolume $ round vol
  return $ augmentEnv
    Volume { vAdj = adj, vCid = cId }


makeVolumeControl = do
  view <- hScaleNew $ adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  id <- onServerConnectionAdd . ever $ widgetSetSensitive view
  view `onDestroy` do onServerConnection $ remove id

  return view


handleVolume = do
  vol <- catchResult 0 (maximum . Map.elems)
  liftIO $ withoutVolumeChange $
    adjustmentSetValue (adj) $ fromIntegral vol
  return True

setVolume vol =
  playbackVolumeGet xmms >>* do
    vols <- catchResult Map.empty id
    lift $ mapM_ ((flip (playbackVolumeSet xmms)) vol) $ Map.keys vols
    return False

withoutVolumeChange =
  bracket_ (signalBlock $ cId) (signalUnblock $ cId)
