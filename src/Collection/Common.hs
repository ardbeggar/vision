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

import Control.Monad

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import UI
import Utils
import Compound

import Collection.ScrollBox
import Collection.ComboModel


data Com
  = Com { eSBox   :: ScrollBox
        , eVPopup :: Menu
        , eCModel :: ListStore ComboItem
        , eScroll :: ScrolledWindow
        , eSAdj   :: Adjustment
        }

coms = ($ ?_Collection_Common)

newtype Wrap a = Wrap { unWrap :: (?_Collection_Common :: Com) => a }

withCommon    = withCommon' . Wrap
withCommon' w = do
  vpopup <- getWidget castToMenu "ui/view-popup"

  sbox   <- mkScrollBox
  cmodel <- mkModel

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowNone
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyNever
  adj <- scrolledWindowGetHAdjustment scroll

  fcRef <- newIORef Nothing

  let scrollIn mfc = void $ withJust mfc $ \fc ->
        flip idleAdd priorityLow $ do
          Rectangle x _ w _ <- widgetGetAllocation fc
          adjustmentClampPage adj (fromIntegral x) (fromIntegral (x + w))
          return False

  adj `afterAdjChanged` do
    mfc <- readIORef fcRef
    scrollIn mfc

  (sBox sbox) `on` setFocusChild $ \mfc -> do
    writeIORef fcRef mfc
    scrollIn mfc

  containerAdd scroll $ outer sbox

  let ?_Collection_Common =
        Com { eSBox   = sbox
            , eVPopup = vpopup
            , eCModel = cmodel
            , eScroll = scroll
            , eSAdj   = adj
            }

  unWrap w


addView w = do
  scrollBoxAdd (coms eSBox) $ outer w
  widgetGrabFocus $ focus w

class FocusChild f where
  type Focus f
  focus :: f -> Focus f

withSelectedView combo f = do
  iter <- comboBoxGetActiveIter combo
  withJust iter $ \iter -> do
    v <- listStoreGetValue (coms eCModel) $ listStoreIterToIndex iter
    f v
