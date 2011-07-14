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
  ( browseCollection
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Monad
import Control.Monad.ToIO
import Control.Monad.Trans
import Control.Monad.W

import Graphics.UI.Gtk hiding (selectAll, focus)

import UI hiding (bindActions)
import Clipboard
import Registry
import XMMS
import Properties
import Builder
import Environment

import Collection.Common
import Collection.List
import Collection.Select
import Collection.Utils


browseCollection _maybeName = runBuilder $ do
  addFromFile $ gladeFilePath "collection-browser"
  builder <- builder
  context <- liftIO $ initUI builder
  let ?context = context

  env <- liftIO $ mkEnv builder

  Just ce <- getEnv clipboardEnv
  W runC  <- runIn ce $> toIO
  bindActions
    [ ("add-to-playlist", envWithColl env $ addToPlaylist False)
    , ("replace-playlist", envWithColl env $ addToPlaylist True)
    , ("select-all", envWithSel env selectAll)
    , ("invert-selection", envWithSel env invertSelection)
    , ("copy", envWithIds env (runC . copyIds))
    , ("edit-properties", envWithIds env showPropertyEditor)
    , ("export-properties", envWithIds env showPropertyExport)
    , ("import-properties", showPropertyImport)
    , ("manage-properties", showPropertyManager)
    , ("save-collection", envWithColl env $ saveCollection)
    , ("rename-collection", envWithNames env $ renameCollection)
    , ("delete-collections", envWithNames env $ deleteCollections)
    ]

  ag <- getObject castToActionGroup "server-actions"
  liftIO $ do
    xcW <- atomically $ newTGWatch connectedV
    tid <- forkIO $ forever $ do
      conn <- atomically $ watch xcW
      postGUISync $ actionGroupSetSensitive ag conn
    window `onDestroy` (killThread tid)

  lv  <- mkListView env
  box <- getObject castToVBox "views"
  liftIO $ do
    boxPackStartDefaults box $ eScroll env
    addView env lv
    onCollBuilt env lv $ mkSelect env

    widgetShowAll window

