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

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import Compound
import Utils
import Environment
import Registry
import Clipboard
import XMMS
import Medialib
import UI

import Properties.Model

import Collection.Common
import Collection.Tracks
import Collection.PropFlt
import Collection.Combo
import Collection.Utils


data VR =
  forall a.
  ( SetColl a
  , ViewItem a
  , FocusChild a
  , WidgetClass (Focus a)
  ) => VR a

data Select
  = S { sCombo   :: ComboBox
      , sColl    :: Coll
      , sBox     :: VBox
      , sEntry   :: Entry
      , sNextRef :: IORef VI
      , sViewRef :: IORef (Maybe VR)
      , sEBox    :: EventBox
      }

instance ViewItem Select where
  nextVIRef = sNextRef

mkSelect ::
  ( WithEnvironment
  , WithRegistry
  , WithClipboard
  , WithXMMS
  , WithMedialib
  , WithUI
  , WithModel
  , WithCommon )
  => Coll
  -> IO Select
mkSelect coll = do
  nextRef <- newIORef None
  box     <- vBoxNew False 5
  combo   <- mkCombo
  entry   <- entryNew
  exp     <- expanderNew "Filter"
  viewRef <- newIORef Nothing
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
        writeIORef viewRef $ Just $ VR w
        setNext s w
        onCollBuilt w mkSelect
        widgetShowAll exp
        boxPackStartDefaults box $ outer w
        widgetGrabFocus $ focus w
      showFilterValid True =
        entry `set` [ secondaryIconStock       := stockApply
                    , secondaryIconTooltipText := "Expression is valid"
                    ]
      showFilterValid False =
        entry `set` [ secondaryIconStock       := stockStop
                    , secondaryIconTooltipText := "Expression is not valid"
                    ]
      mkFilterColl = do
        text <- entryGetText entry
        case text of
          [] -> do
            showFilterValid True
            return coll
          _  -> do
            flt <- collParse text
            int <- collNew TypeIntersection
            collAddOperand int coll
            collAddOperand int flt
            showFilterValid True
            return int

  eventBoxSetVisibleWindow eBox False
  containerAdd eBox box

  boxPackStart box combo PackNatural 0
  combo `on` changed $ handleXMMSException $
    withSelectedView combo $ \cur -> case cur of
      CITracks    -> setup =<< mkTrackView  =<< mkFilterColl
      CIProp pr   -> setup =<< mkPropFlt pr =<< mkFilterColl
      CISeparator -> return ()

  showFilterValid True
  containerAdd exp entry
  boxPackStart box exp PackNatural 0

  widgetShowAll eBox

  let filter = do
        vr <- readIORef viewRef
        withJust vr $ \(VR w) -> do
          killNext w
          (mkFilterColl >>= setColl w) `catchXMMS_` (showFilterValid False)

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

  exp `onActivate` do
    e <- exp `get` expanderExpanded
    unless e $ widgetGrabFocus entry

  eBox `on` keyPressEvent $ tryEvent $ do
    "l"       <- eventKeyName
    [Control] <- eventModifier
    liftIO $ do
      expanderSetExpanded exp True
      realized <- widgetGetRealized entry
      if realized
        then widgetGrabFocus entry
        else void $ mfix $ \cid ->
          entry `after` realize $ do
            signalDisconnect cid
            widgetGrabFocus entry

  eBox `on` keyPressEvent $ tryEvent $ do
    "j"       <- eventKeyName
    [Control] <- eventModifier
    liftIO $ widgetGrabFocus combo

  eBox `on` keyPressEvent $ tryEvent $ do
    "k"       <- eventKeyName
    [Control] <- eventModifier
    liftIO $ do
      Just (VR w) <- readIORef viewRef
      widgetGrabFocus $ focus w

  return s

instance CompoundWidget Select where
  type Outer Select = EventBox
  outer = sEBox

instance FocusChild Select where
  type Focus Select = ComboBox
  focus = sCombo
