-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

module Collection.Select
  ( Select (..)
  , mkSelect
  , killSelect
  ) where

import Data.IORef

import Graphics.UI.Gtk

import XMMS2.Client

import Utils

import Collection.Tracks
import Collection.PropFlt
import Collection.Combo
import Collection.ScrollBox hiding (sBox)


data Select
  = S { sCombo :: ComboBox
      , sColl  :: Coll
      , sBox   :: VBox
      , sKill  :: IORef (Maybe (IO ()))
      , sKillS :: IORef (Maybe (IO ()))
      }

mkSelect abRef sbox cmod coll = do
  kill  <- newIORef Nothing
  killS <- newIORef Nothing
  box   <- vBoxNew False 5
  combo <- mkCombo cmod
  boxPackStart box combo PackNatural 0

  combo `on` changed $ do
    iter <- comboBoxGetActiveIter combo
    withJust iter $ \iter -> do
      maybeKillS <- readIORef killS
      withJust maybeKillS id
      maybeKill <- readIORef kill
      withJust maybeKill id
      writeIORef kill Nothing
      sel <- listStoreGetValue cmod $ listStoreIterToIndex iter
      case sel of
        CITracks -> do
          tv <- makeTrackView abRef
          writeIORef killS $ Just $ widgetDestroy $ tScroll tv
          onTracksSelected tv $ \coll -> do
            maybeKill <- readIORef kill
            withJust maybeKill id
            sel <- mkSelect abRef sbox cmod coll
            writeIORef kill $ Just $ killSelect sel
            scrollBoxAdd sbox $ sBox sel
            widgetGrabFocus $ sCombo sel
          boxPackStartDefaults box $ tScroll tv
          widgetShowAll $ tScroll tv
          widgetGrabFocus $ tView tv
          loadTracks tv coll
        CIProp pr -> do
          pf <- mkPropFlt abRef pr coll
          writeIORef killS $ Just $ widgetDestroy $ pScroll pf
          onPropsSelected pf $ \coll -> do
            maybeKill <- readIORef kill
            withJust maybeKill id
            sel <- mkSelect abRef sbox cmod coll
            writeIORef kill $ Just $ killSelect sel
            scrollBoxAdd sbox $ sBox sel
            widgetGrabFocus $ sCombo sel
          boxPackStartDefaults box $ pScroll pf
          widgetGrabFocus $ pView pf

  widgetShowAll box
  return S { sCombo = combo
           , sBox   = box
           , sColl  = coll
           , sKill  = kill
           , sKillS = killS
           }

killSelect sel = do
  maybeKill <- readIORef $ sKill sel
  withJust maybeKill id
  widgetDestroy $ sBox sel
