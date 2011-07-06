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
  ) where

import Graphics.UI.Gtk

import XMMS2.Client

import Utils

import Collection.Tracks
import Collection.PropFlt
import Collection.Combo


data Select
  = S { sCombo :: ComboBox
      , sColl  :: Coll
      , sBox   :: VBox
      }

mkSelect cmod coll = do
  box   <- vBoxNew False 5
  combo <- mkCombo cmod
  boxPackStart box combo PackNatural 0

  combo `on` changed $ do
    iter <- comboBoxGetActiveIter combo
    withJust iter $ \iter -> do
      sel <- listStoreGetValue cmod $ listStoreIterToIndex iter
      case sel of
        Nothing -> do
          tv <- makeTrackView
          boxPackStartDefaults box $ tScroll tv
          widgetShowAll $ tScroll tv
          loadTracks tv coll
        Just pr -> do
          pf <- mkPropFlt pr coll
          boxPackStartDefaults box $ pScroll pf

  widgetShowAll box
  return S { sCombo = combo
           , sBox   = box
           , sColl  = coll
           }
