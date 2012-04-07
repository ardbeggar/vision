-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

module Volume
  ( initVolume
  , volumeEnv
  , WithVolume
  , withVolume
  , makeVolumeControl
  ) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import qualified Data.Map as Map
import Data.Typeable
import Data.Env
import Data.Maybe

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client

import XMMS
import Utils
import Registry


data Ix = Ix deriving (Typeable)
deriving instance Typeable Adjustment

volumeEnv :: Extract Ix Adjustment
volumeEnv = Extract

initVolume :: WithRegistry => IO ()
initVolume = withXMMS $ do
  adj <- adjustmentNew 0 0 100 5 5 0
  addEnv Ix adj

  cId <- adj `onValueChanged` do
    vol <- adjustmentGetValue adj
    setVolume $ round vol

  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    if conn
      then do
      playbackVolumeGet xmms >>* do
        handleVolume adj cId
        liftIO $ broadcastPlaybackVolumeChanged xmms >>* do
          handleVolume adj cId
          return True
      else withoutVolumeChange cId $ adjustmentSetValue adj 0

  return ()

type WithVolume = ?_Volume :: Adjustment

withVolume :: WithRegistry => (WithVolume => IO a) -> IO a
withVolume func = do
  Just (Env v) <- getEnv volumeEnv
  let ?_Volume = v
  func

makeVolumeControl :: (WithXMMS, WithVolume) => IO HScale
makeVolumeControl = do
  view <- hScaleNew ?_Volume
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  xcW <- atomically $ newTGWatch connectedV
  tid <- forkIO $ forever $ do
    conn <- atomically $ watch xcW
    widgetSetSensitive view conn
  view `onDestroy` killThread tid

  return view

handleVolume :: Adjustment -> ConnectId Adjustment -> ResultM c (Maybe (Dict Int32)) ()
handleVolume adj cId = do
  vol <- catchResult 0 (maybe 0 (maximum . Map.elems))
  liftIO $ withoutVolumeChange cId $
    adjustmentSetValue adj $ fromIntegral vol

setVolume :: WithXMMS => Int -> IO ()
setVolume vol =
  playbackVolumeGet xmms >>* do
    vols <- catchResult Map.empty (fromMaybe Map.empty)
    liftIO $ mapM_ (flip (playbackVolumeSet xmms) vol) $ Map.keys vols

withoutVolumeChange :: ConnectId Adjustment -> IO a -> IO a
withoutVolumeChange cId =
  bracket_ (signalBlock cId) (signalUnblock cId)
