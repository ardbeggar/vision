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
      , sEntry   :: Entry
      , sNextRef :: IORef VI
      }

instance ViewItem Select where
  nextVIRef = sNextRef

mkSelect coll = do
  nextRef <- newIORef None
  box     <- vBoxNew False 5
  combo   <- mkCombo
  entry   <- entryNew
  exp     <- expanderNew "Filter"
  let s = S { sCombo   = combo
            , sBox     = box
            , sColl    = coll
            , sEntry   = entry
            , sNextRef = nextRef
            }
      setup w = do
          setNext s w
          onCollBuilt w mkSelect
          widgetShowAll exp
          boxPackStartDefaults box $ outer w
          widgetGrabFocus $ focus w
      mkFilterColl = do
        text <- entryGetText entry
        case text of
          [] -> return coll
          _  -> do
            flt <- collParse text
            int <- collNew TypeIntersection
            collAddOperand int coll
            collAddOperand int flt
            return int

  boxPackStart box combo PackNatural 0
  combo `on` changed $ handleXMMSException $
    withSelectedView combo $ \cur -> case cur of
      CITracks    -> setup =<< mkTrackView  =<< mkFilterColl
      CIProp pr   -> setup =<< mkPropFlt pr =<< mkFilterColl
      CISeparator -> return ()

  widgetShowAll box

  entry `on` entryActivate $ handleXMMSException $
    withSelectedView combo $ \cur -> case cur of
      CITracks    -> setup =<< mkTrackView  =<< mkFilterColl
      CIProp pr   -> setup =<< mkPropFlt pr =<< mkFilterColl
      CISeparator -> return ()

  containerAdd exp entry
  boxPackStart box exp PackNatural 0

  return s

instance CompoundWidget Select where
  type Outer Select = VBox
  outer = sBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
