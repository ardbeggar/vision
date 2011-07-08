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

module Collection
  ( initCollection
  , browseCollection
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Monad
import Control.Monad.ToIO
import Control.Monad.Trans

import Graphics.UI.Gtk hiding (selectAll, focus)

import UI
import Clipboard
import Context
import XMMS
import Properties
import Compound

import Collection.Common
import Collection.List
import qualified Collection.Select as S
import Collection.Utils


initCollection =
  initList

browseCollection _maybeName = do
  builder <- liftIO $ makeBuilder "collection-browser"
  context <- liftIO $ initUI builder
  let ?context = context

  env <- liftIO $ mkEnv builder

  Just ce <- getEnv clipboardEnv
  runEnvT ce $ runIn clipboardEnv $> do
    io $ \run -> bindActions builder $
      [ ("add-to-playlist", envWithColl env $ addToPlaylist False)
      , ("replace-playlist", envWithColl env $ addToPlaylist True)
      , ("select-all", envWithSel env selectAll)
      , ("invert-selection", envWithSel env invertSelection)
      , ("copy", envWithIds env (run . copyIds))
      , ("edit-properties", envWithIds env showPropertyEditor)
      , ("export-properties", envWithIds env showPropertyExport)
      , ("import-properties", showPropertyImport)
      , ("manage-properties", showPropertyManager)
      , ("save-collection", envWithColl env $ saveCollection)
      , ("rename-collection", envWithNames env $ renameCollection)
      , ("delete-collections", envWithNames env $ deleteCollections)
      ]

  liftIO $ do
    ag  <- builderGetObject builder castToActionGroup "server-actions"
    xcW <- atomically $ newTGWatch connectedV
    tid <- forkIO $ forever $ do
      conn <- atomically $ watch xcW
      postGUISync $ actionGroupSetSensitive ag conn
    window `onDestroy` (killThread tid)

  lv <- mkListView env
  liftIO $ do
    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType scroll ShadowNone
    scrolledWindowSetPolicy scroll PolicyAutomatic PolicyNever
    adj <- scrolledWindowGetHAdjustment scroll
    adj `afterAdjChanged` do
      max <- adjustmentGetUpper adj
      pgs <- adjustmentGetPageSize adj
      adjustmentSetValue adj $ max - pgs

    box <- builderGetObject builder castToVBox "views"
    boxPackStartDefaults box scroll

    containerAdd scroll $ outer $ eSBox env

    addView env lv

    onCollBuilt lv $ \coll -> do
      s <- S.mkSelect env coll
      setNext lv s
      addView env s

    widgetShowAll window

