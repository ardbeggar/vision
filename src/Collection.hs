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

import Graphics.UI.Gtk hiding (selectAll, focus)

import UI
import Clipboard
import XMMS
import Properties
import Builder
import Environment
import Medialib

import Collection.Common
import Collection.List
import Collection.Select
import Collection.Utils


browseCollection _maybeName = withBuilder $ do
  addFromFile $ gladeFilePath "collection-browser"
  withUI "Vision Collection Browser" $
    withCommon $ withClipboard $ withMedialib $ do
      bindActions
        [ ("import-properties", showPropertyImport)
        , ("manage-properties", showPropertyManager)
        ]

      ag <- getObject castToActionGroup "server-actions"
      xcW <- atomically $ newTGWatch connectedV
      tid <- forkIO $ forever $ do
        conn <- atomically $ watch xcW
        postGUISync $ actionGroupSetSensitive ag conn
      window `onDestroy` (killThread tid)

      lv  <- mkListView
      box <- getObject castToVBox "views"
      boxPackStartDefaults box $ coms eScroll

      addView lv

      onCollBuilt lv mkSelect

      widgetShowAll window

