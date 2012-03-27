-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 27 Mar. 2012
--
--  Copyright (C) 2012 Oleg Belozeorov
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

module Library
  ( browseLibrary
  ) where

import Graphics.UI.Gtk hiding (selectAll, focus)

import UI
import Clipboard
import Properties
import Builder
import Environment
import Medialib

import Collection.Utils


browseLibrary = withBuilder $ do
  addFromFile $ gladeFilePath "library-browser"
  withUI "Vision Library Browser" $
    withClipboard $ withMedialib $ do
      bindActions
        [ ("import-properties", showPropertyImport)
        , ("manage-properties", showPropertyManager)
        ]

      ag <- getObject castToActionGroup "server-actions"
      watchConnectionState window (postGUISync . actionGroupSetSensitive ag)

      _box <- getObject castToVBox "views"

      widgetShowAll window

