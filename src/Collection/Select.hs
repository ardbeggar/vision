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
  = S { sCombo   :: ComboBox
      , sColl    :: Coll
      , sBox     :: VBox
      , sNextRef :: IORef VI
      }

instance ViewItem Select where
  nextVIRef = sNextRef

mkSelect env coll = do
  nextRef <- newIORef None
  box     <- vBoxNew False 5
  combo   <- mkCombo env
  let s = S { sCombo  = combo
            , sBox     = box
            , sColl    = coll
            , sNextRef = nextRef
            }

  boxPackStart box combo PackNatural 0

  let setup w = do
        setNext s w
        onCollBuilt env w $ mkSelect env
        boxPackStartDefaults box $ outer w
        widgetGrabFocus $ focus w

  combo `on` changed $ do
    iter <- comboBoxGetActiveIter combo
    withJust iter $ \iter -> do
      cur <- listStoreGetValue (eCModel env) $ listStoreIterToIndex iter
      case cur of
        CITracks    -> setup =<< mkTrackView env coll
        CIProp pr   -> setup =<< mkPropFlt env pr coll
        CISeparator -> return ()

  widgetShowAll box
  return s

instance CompoundWidget Select where
  type Outer Select = VBox
  outer = sBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
