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
import Control.Monad.EnvIO

import Graphics.UI.Gtk hiding (selectAll, focus)

import UI
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


browseCollection _maybeName = flip runEnvIO () $ withBuilder $ do
  addFromFile $ gladeFilePath "collection-browser"
  withUI $ runCommon $ do
    Just cb <- getEnv clipboardEnv
    W runCB <- runIn cb $> toIO
    W runCM <- runIn commonEnv $> toIO
    bindActions
      [ ("add-to-playlist", runCM $ comWithColl $ addToPlaylist False)
      , ("replace-playlist", runCM $ comWithColl $ addToPlaylist True)
      , ("select-all", runCM $ comWithSel selectAll)
      , ("invert-selection", runCM $ comWithSel invertSelection)
      , ("copy", runCM $ comWithIds (runCB . copyIds))
      , ("edit-properties", runCM $ comWithIds $ showPropertyEditor)
      , ("export-properties", runCM $ comWithIds $ showPropertyExport)
      , ("import-properties", showPropertyImport)
      , ("manage-properties", showPropertyManager)
      , ("save-collection", runCM $ comWithColl $ saveCollection)
      , ("rename-collection", runCM $ comWithNames $ renameCollection)
      , ("delete-collections", runCM $ comWithNames $ deleteCollections)
      ]

    ag <- getObject castToActionGroup "server-actions"
    liftIO $ do
      xcW <- atomically $ newTGWatch connectedV
      tid <- forkIO $ forever $ do
        conn <- atomically $ watch xcW
        postGUISync $ actionGroupSetSensitive ag conn
      window `onDestroy` (killThread tid)

    lv     <- mkListView
    box    <- getObject castToVBox "views"
    scroll <- coms eScroll
    liftIO $ boxPackStartDefaults box scroll

    addView lv

    W run <- toIO
    onCollBuilt lv $ run . mkSelect

    liftIO $ widgetShowAll window

