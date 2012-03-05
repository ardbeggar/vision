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

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import Compound
import Utils

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
      , sEBox    :: EventBox
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
  eBox    <- eventBoxNew
  let s = S { sCombo   = combo
            , sBox     = box
            , sColl    = coll
            , sEntry   = entry
            , sNextRef = nextRef
            , sViewRef = viewRef
            , sEBox    = eBox
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

  eventBoxSetVisibleWindow eBox False
  containerAdd eBox box

  boxPackStart box combo PackNatural 0
  combo `on` changed $ handleXMMSException $
    withSelectedView combo $ \cur -> case cur of
      CITracks    -> setup =<< mkTrackView  =<< mkFilterColl
      CIProp pr   -> setup =<< mkPropFlt pr =<< mkFilterColl
      CISeparator -> return ()

  widgetShowAll eBox

  let filter = do
        vr <- readIORef viewRef
        case vr of
          VR v -> do
            killNext v
            (mkFilterColl >>= setColl v) `catch`
              \(_ :: XMMSException) -> return ()
          _    -> return ()

  hRef <- newIORef Nothing
  entry `on` editableChanged $ do
    hid <- readIORef hRef
    withJust (hid) timeoutRemove
    hid <- timeoutAddFull (filter >> return False) priorityLow 250
    writeIORef hRef $ Just hid

  entry `on` entryActivate $ do
    hid <- readIORef hRef
    withJust (hid) timeoutRemove
    writeIORef hRef Nothing
    filter

  containerAdd exp entry
  boxPackStart box exp PackNatural 0

  exp `onActivate` do
    e <- exp `get` expanderExpanded
    unless e $ widgetGrabFocus entry

  eBox `on` keyPressEvent $ tryEvent $ do
    "l"       <- eventKeyName
    [Control] <- eventModifier
    liftIO $ do
      expanderSetExpanded exp True
      widgetGrabFocus entry

  return s

instance CompoundWidget Select where
  type Outer Select = EventBox
  outer = sEBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
