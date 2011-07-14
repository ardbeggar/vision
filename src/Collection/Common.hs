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
  ( Com (..)
  , commonEnv
  , coms
  , runCommon
  , comWithColl
  , comWithIds
  , comWithSel
  , comWithNames
  , addView
  , FocusChild (..)
  , withSelectedView
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.EnvIO

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import UIEnvIO
import Utils
import XMMS
import Compound
import Builder

import Collection.Actions
import Collection.ScrollBox
import Collection.ComboModel


data Ix = Ix

data Com
  = Com { eABRef  :: IORef ActionBackend
        , eAE     :: ActionEnabler
        , eSBox   :: ScrollBox
        , eLPopup :: Menu
        , eVPopup :: Menu
        , eCModel :: ListStore ComboItem
        , eScroll :: ScrolledWindow
        , eSAdj   :: Adjustment
        }

commonEnv :: Extract Ix Com
commonEnv = Extract

coms = envsx Ix

runCommon f = do
  abRef <- liftIO $ newIORef emptyAB

  selActs <- actions [ "add-to-playlist"
                     , "replace-playlist"
                     , "copy"
                     , "edit-properties"
                     , "export-properties"
                     , "save-collection"
                     ]
  renAct <- action "rename-collection"
  delAct <- action "delete-collections"
  let ae = AE { aEnableSel = \en -> mapM_ (`actionSetSensitive` en) selActs
              , aEnableRen = actionSetSensitive renAct
              , aEnableDel = actionSetSensitive delAct
              }

  lpopup <- getWidget castToMenu "ui/list-popup"
  vpopup <- getWidget castToMenu "ui/view-popup"

  env <- liftIO $ do
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

    return $ mkEnv Ix
      Com { eABRef  = abRef
          , eAE     = ae
          , eSBox   = sbox
          , eLPopup = lpopup
          , eVPopup = vpopup
          , eCModel = cmodel
          , eScroll = scroll
          , eSAdj   = adj
          }

  runIn' (env :*:) $> f


withABRef f = do
  abRef <- coms eABRef
  liftIO $ f abRef

comWithColl f =
  withABRef $ \r -> do
    ab <- readIORef r
    aWithColl ab f

comWithIds f =
  comWithColl $ \coll ->
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ f ids

comWithSel f =
  withABRef $ \r -> do
    ab <- readIORef r
    withJust (aSelection ab) f

comWithNames f =
  withABRef $ \r -> do
    ab <- readIORef r
    aWithNames ab f

addView w = do
  sbox <- coms eSBox
  liftIO $ do
    scrollBoxAdd sbox $ outer w
    widgetGrabFocus $ focus w

class FocusChild f where
  type Focus f
  focus :: f -> Focus f

withSelectedView combo f = do
  store <- coms eCModel
  io $ \run -> do
    iter <- comboBoxGetActiveIter combo
    withJust iter $ \iter -> do
      v <- listStoreGetValue store $ listStoreIterToIndex iter
      run $ f v
