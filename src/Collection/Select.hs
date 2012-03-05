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

{-# LANGUAGE NoMonoLocalBinds, ExistentialQuantification #-}

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


data VR =
  forall a. (SetColl a, ViewItem a) => VR a
  | NoVR

data Select
  = S { sCombo   :: ComboBox
      , sColl    :: Coll
      , sBox     :: VBox
      , sEntry   :: Entry
      , sNextRef :: IORef VI
      , sViewRef :: IORef VR
      }

instance ViewItem Select where
  nextVIRef = sNextRef

mkSelect coll = do
  nextRef <- newIORef None
  box     <- vBoxNew False 5
  combo   <- mkCombo
  entry   <- entryNew
  exp     <- expanderNew "Filter"
  viewRef <- newIORef NoVR
  let s = S { sCombo   = combo
            , sBox     = box
            , sColl    = coll
            , sEntry   = entry
            , sNextRef = nextRef
            , sViewRef = viewRef
            }
      setup w = do
        writeIORef viewRef $ VR w
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

  entry `on` entryActivate $ handleXMMSException $ do
    vr <- readIORef viewRef
    case vr of
      VR v -> do
        killNext v
        fc <- mkFilterColl
        setColl v fc
      _    -> return ()

  containerAdd exp entry
  boxPackStart box exp PackNatural 0

  return s

instance CompoundWidget Select where
  type Outer Select = VBox
  outer = sBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
