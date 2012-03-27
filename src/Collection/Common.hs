-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

{-# LANGUAGE Rank2Types #-}

module Collection.Common
  ( Com (..)
  , coms
  , withCommon
  , addView
  , FocusChild (..)
  , withSelectedView
  ) where

import Graphics.UI.Gtk hiding (focus)

import UI
import Utils
import Compound

import Widgets.ColumnView

import Collection.ComboModel


data Com
  = Com { eCV     :: ColumnView
        , eVPopup :: Menu
        , eCModel :: ListStore ComboItem
        , eScroll :: ScrolledWindow
        }

coms = ($ ?_Collection_Common)

newtype Wrap a = Wrap { unWrap :: (?_Collection_Common :: Com) => a }

withCommon    = withCommon' . Wrap
withCommon' w = do
  vpopup <- getWidget castToMenu "ui/view-popup"

  cv     <- columnViewNew
  cmodel <- mkModel

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowNone
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyNever
  adj <- scrolledWindowGetHAdjustment scroll
  columnViewSetupFocusScroll cv adj
  containerAdd scroll $ outer cv

  let ?_Collection_Common =
        Com { eCV     = cv
            , eVPopup = vpopup
            , eCModel = cmodel
            , eScroll = scroll
            }

  unWrap w


addView w = do
  columnViewAdd (coms eCV) $ outer w
  widgetGrabFocus $ focus w

class FocusChild f where
  type Focus f
  focus :: f -> Focus f

withSelectedView combo f = do
  iter <- comboBoxGetActiveIter combo
  withJust iter $ \iter -> do
    v <- listStoreGetValue (coms eCModel) $ listStoreIterToIndex iter
    f v
