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

  com <- liftIO $ mkCom builder

  Just ce <- getEnv clipboardEnv
  W runC  <- runIn ce $> toIO
  bindActions
    [ ("add-to-playlist", comWithColl com $ addToPlaylist False)
    , ("replace-playlist", comWithColl com $ addToPlaylist True)
    , ("select-all", comWithSel com selectAll)
    , ("invert-selection", comWithSel com invertSelection)
    , ("copy", comWithIds com (runC . copyIds))
    , ("edit-properties", comWithIds com showPropertyEditor)
    , ("export-properties", comWithIds com showPropertyExport)
    , ("import-properties", showPropertyImport)
    , ("manage-properties", showPropertyManager)
    , ("save-collection", comWithColl com $ saveCollection)
    , ("rename-collection", comWithNames com $ renameCollection)
    , ("delete-collections", comWithNames com $ deleteCollections)
    ]

  ag <- getObject castToActionGroup "server-actions"
  liftIO $ do
    xcW <- atomically $ newTGWatch connectedV
    tid <- forkIO $ forever $ do
      conn <- atomically $ watch xcW
      postGUISync $ actionGroupSetSensitive ag conn
    window `onDestroy` (killThread tid)

  lv  <- mkListView com
  box <- getObject castToVBox "views"
  liftIO $ do
    boxPackStartDefaults box $ eScroll com
    addView com lv
    onCollBuilt com lv $ mkSelect com

    widgetShowAll window

