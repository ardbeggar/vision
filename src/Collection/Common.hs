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

module Collection.Common
  ( Env (..)
  , mkEnv
  , envWithColl
  , envWithIds
  , envWithSel
  , envWithNames
  , addView
  , FocusChild (..)
  ) where

import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import UI
import Utils
import XMMS
import Compound

import Collection.Actions
import Collection.ScrollBox
import Collection.ComboModel


data Env
  = Env { eABRef  :: IORef ActionBackend
        , eAE     :: ActionEnabler
        , eSBox   :: ScrollBox
        , eLPopup :: Menu
        , eVPopup :: Menu
        , eCModel :: ListStore ComboItem
        , eScroll :: ScrolledWindow
        , eSAdj   :: Adjustment
        }

mkEnv builder = do
  abRef <- newIORef emptyAB
  selActs <- mapM (action builder)
             [ "add-to-playlist"
             , "replace-playlist"
             , "copy"
             , "edit-properties"
             , "export-properties"
             , "save-collection"
             ]
  renAct <- action builder "rename-collection"
  delAct <- action builder "delete-collections"
  let ae = AE { aEnableSel = \en -> mapM_ (`actionSetSensitive` en) selActs
              , aEnableRen = actionSetSensitive renAct
              , aEnableDel = actionSetSensitive delAct
              }
  sbox <- mkScrollBox
  lpopup <- liftIO $ getWidget castToMenu "ui/list-popup"
  vpopup <- liftIO $ getWidget castToMenu "ui/view-popup"
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

  return Env { eABRef  = abRef
             , eAE     = ae
             , eSBox   = sbox
             , eLPopup = lpopup
             , eVPopup = vpopup
             , eCModel = cmodel
             , eScroll = scroll
             , eSAdj   = adj
             }

envWithColl env f = do
  ab <- readIORef $ eABRef env
  aWithColl ab f

envWithIds env f = envWithColl env $ \coll ->
  collQueryIds xmms coll [] 0 0 >>* do
    ids <- result
    liftIO $ f ids

envWithSel env f = do
  ab <- readIORef $ eABRef env
  withJust (aSelection ab) f

envWithNames env f = do
  ab <- readIORef $ eABRef env
  aWithNames ab f

addView env w = do
  scrollBoxAdd (eSBox env) $ outer w
  widgetGrabFocus $ focus w

class FocusChild f where
  type Focus f
  focus :: f -> Focus f
