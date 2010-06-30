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

import Graphics.UI.Gtk hiding (add)

import UI
import XMMS
import Handler
import Utils
import Location.Model
import Location.View
import Location.Control


setupUI browse = do
  addUIActions uiActions

  srvAG <- actionGroupNew "server"
  actionGroupAddActions srvAG $ srvActions browse
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
  toolbarInsert toolbar item 4

  load <- getAction srvAG "load"
  locationEntry `onEntryActivate` do
    actionActivate load

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  containerAdd scroll locationView
  boxPackStartDefaults contents scroll

  popup <- getWidget castToMenu "ui/location-popup"
  setupTreeViewPopup locationView popup

  down <- getAction srvAG "down"
  locationView `onRowActivated` \_ _ -> do
    actionActivate down

  back    <- getAction srvAG "back"
  forward <- getAction srvAG "forward"
  up      <- getAction srvAG "up"
  refresh <- getAction srvAG "refresh"
  let updateN = do
        (eb, ef, eu, er) <- canGo
        actionSetSensitive back eb
        actionSetSensitive forward ef
        actionSetSensitive up eu
        actionSetSensitive refresh er
  onLocation . add . ever . const $ updateN
  updateN

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
  , ActionEntry
    { actionEntryName        = "open-location"
    , actionEntryLabel       = "_Open location"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Just "<Control>l"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = openLocation
    }
  , ActionEntry
    { actionEntryName        = "go"
    , actionEntryLabel       = "_Go"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "location-popup"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  ]

srvActions browse =
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
    , actionEntryCallback    = loadAtCursor (loadLocation . Go)
    }
  , ActionEntry
    { actionEntryName        = "new-window"
    , actionEntryLabel       = "_New window"
    , actionEntryStockId     = Just stockNew
    , actionEntryAccelerator = Just "<Control>n"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = browse Nothing
    }
  , ActionEntry
    { actionEntryName        = "browse-in-new-window"
    , actionEntryLabel       = "_Browse in new window"
    , actionEntryStockId     = Just stockNew
    , actionEntryAccelerator = Just "<Control>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadAtCursor (browse . Just)
    }
  , ActionEntry
    { actionEntryName        = "add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Just "<Control>p"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = addToPlaylist
    }
  , ActionEntry
    { actionEntryName        = "replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Just stockRefresh
    , actionEntryAccelerator = Just "<Control><Shift>p"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = replacePlaylist
    }
  , ActionEntry
    { actionEntryName        = "back"
    , actionEntryLabel       = "Go _back"
    , actionEntryStockId     = Just stockGoBack
    , actionEntryAccelerator = Just "<Alt>Left"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadLocation Back
    }
  , ActionEntry
    { actionEntryName        = "forward"
    , actionEntryLabel       = "Go _forward"
    , actionEntryStockId     = Just stockGoForward
    , actionEntryAccelerator = Just "<Alt>Right"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadLocation Forward
    }
  , ActionEntry
    { actionEntryName        = "up"
    , actionEntryLabel       = "Go _up"
    , actionEntryStockId     = Just stockGoUp
    , actionEntryAccelerator = Just "<Alt>Up"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadLocation Up
    }
  , ActionEntry
    { actionEntryName        = "refresh"
    , actionEntryLabel       = "_Refresh"
    , actionEntryStockId     = Just stockRefresh
    , actionEntryAccelerator = Just "F5"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = loadLocation Refresh
    }
  ]

loadCurrentLocation = do
  text <- trim <$> entryGetText locationEntry
  case text of
    [] -> do
      cur <- getCurrentLocation
      case cur of
        [] -> return ()
        _  -> do
          entrySetText locationEntry cur
          widgetGrabFocus locationView
    _  -> loadLocation . Go $ makeURL text

loadAtCursor func = do
  (path, _) <- treeViewGetCursor locationView
  case path of
    [n] -> do
      item <- listStoreGetValue locationStore n
      when (iIsDir item) $ func $ iPath item
    _   ->
      return ()
