-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
--
--  Copyright (C) 2010 Oleg Belozeorov
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

module Playlist.UI
  ( setupUI
  ) where

import Control.Applicative
import Data.Maybe

import Graphics.UI.Gtk

import UI
import Environment
import Playlist.View


setupUI = do
  uim <- uiManagerNew
  windowAddAccelGroup window =<< uiManagerGetAccelGroup uim

  uiAG <- actionGroupNew "ui"
  actionGroupAddActions uiAG uiActions
  uiManagerInsertActionGroup uim uiAG 1
  uiManagerAddUiFromFile uim $ uiFilePath "playlist"

  box <- vBoxNew False 0
  containerAdd window box

  menubar <- castToMenuBar . fromJust <$> uiManagerGetWidget uim "ui/menubar"
  boxPackStart box menubar PackNatural 0

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  containerAdd scroll playlistView
  boxPackStartDefaults box scroll

  window `onDestroy` mainQuit


uiActions =
  [ ActionEntry
    { actionEntryName        = "menubar"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "music"
    , actionEntryLabel       = "_Music"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "quit"
    , actionEntryLabel       = "_Quit"
    , actionEntryStockId     = Just "gtk-quit"
    , actionEntryAccelerator = Just "<Control>q"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = mainQuit
    }
  ]
