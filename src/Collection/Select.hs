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

{-# LANGUAGE NoMonoLocalBinds #-}

module Collection.Select
  ( Select (..)
  , mkSelect
  , killSelect
  ) where

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import Utils
import Compound

import Collection.Common
import Collection.Tracks
import Collection.PropFlt
import Collection.Combo
import Collection.Utils


data Select
  = S { sCombo :: ComboBox
      , sColl  :: Coll
      , sBox   :: VBox
      , sKill  :: IORef (Maybe (IO ()))
      , sKillS :: IORef (Maybe (IO ()))
      }

mkSelect env coll = do
  kill  <- newIORef Nothing
  killS <- newIORef Nothing
  box   <- vBoxNew False 5
  combo <- mkCombo env
  boxPackStart box combo PackNatural 0

  let setup w = do
        writeIORef killS $ Just $ widgetDestroy $ outer w
        onCollBuilt w $ \coll -> do
          maybeKill <- readIORef kill
          withJust maybeKill id
          sel <- mkSelect env coll
          writeIORef kill $ Just $ killSelect sel
          addView env sel
        boxPackStartDefaults box $ outer w
        widgetGrabFocus $ focus w

  combo `on` changed $ do
    iter <- comboBoxGetActiveIter combo
    withJust iter $ \iter -> do
      maybeKillS <- readIORef killS
      withJust maybeKillS id
      maybeKill <- readIORef kill
      withJust maybeKill id
      writeIORef kill Nothing
      cur <- listStoreGetValue (eCModel env) $ listStoreIterToIndex iter
      case cur of
        CITracks    -> setup =<< mkTrackView env coll
        CIProp pr   -> setup =<< mkPropFlt env pr coll
        CISeparator -> return ()

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

instance CompoundWidget Select where
  type Outer Select = VBox
  outer = sBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
