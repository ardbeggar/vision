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
  ) where

import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk

import XMMS2.Client

import UI
import Utils
import XMMS

import Collection.Actions


data Env
  = Env { eABRef :: IORef ActionBackend
        , eAE    :: ActionEnabler
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
  return Env { eABRef = abRef
             , eAE    = ae
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
