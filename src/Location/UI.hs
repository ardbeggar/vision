-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
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

module Location.UI
  ( setupUI
  ) where

import Control.Applicative
import Control.Monad

import Graphics.UI.Gtk

import UI
import XMMS
import Handler
import Utils
import Location.Model
import Location.View
import Location.Control


setupUI = do
  addUIActions uiActions

  srvAG <- actionGroupNew "server"
  actionGroupAddActions srvAG srvActions
  onServerConnectionAdd . ever $ actionGroupSetSensitive srvAG
  insertActionGroup srvAG 1

  addUIFromFile "location-browser"

  toolbar <- getWidget castToToolbar "ui/toolbar"
  toolbarSetStyle toolbar ToolbarIcons
  boxPackStart contents toolbar PackNatural 0

  item <- toolItemNew
  toolItemSetHomogeneous item False
  toolItemSetExpand item True
  containerAdd item locationEntry
  toolbarInsert toolbar item 0

  load <- getAction srvAG "load"
  locationEntry `onEntryActivate` do
    actionActivate load

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  containerAdd scroll locationView
  boxPackStartDefaults contents scroll

  down <- getAction srvAG "down"
  locationView `onRowActivated` \_ _ -> do
    actionActivate down

  updateWindowTitle

  return ()


uiActions =
  [ ActionEntry
    { actionEntryName        = "location"
    , actionEntryLabel       = "_Location"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  ]

srvActions =
  [ ActionEntry
    { actionEntryName        = "load"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Just stockJumpTo
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadCurrentLocation
    }
  , ActionEntry
    { actionEntryName        = "down"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadAtCursor
    }
  , ActionEntry
    { actionEntryName        = "open-location"
    , actionEntryLabel       = "_Open location"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Just "<Control>l"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = openLocation
    }
  , ActionEntry
    { actionEntryName        = "add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Just "<Control>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = addToPlaylist
    }
  , ActionEntry
    { actionEntryName        = "replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Just stockRefresh
    , actionEntryAccelerator = Just "<Control><Shift>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = replacePlaylist
    }
  ]

loadCurrentLocation = do
  text <- trim <$> entryGetText locationEntry
  url  <- case text of
    [] -> getCurrentLocation
    _  -> return $ makeURL text
  unless (null url) $ loadLocation url

loadAtCursor = do
  (path, _) <- treeViewGetCursor locationView
  case path of
    [n] -> do
      item <- listStoreGetValue locationStore n
      when (iIsDir item) $ loadLocation $ iPath item
    _   ->
      return ()
