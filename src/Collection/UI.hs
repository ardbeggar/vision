-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection.UI
  ( setupUI
  ) where

import Graphics.UI.Gtk hiding (add)

import UI
import XMMS
import Handler
import Collection.View
import Collection.List


setupUI browse = do
  addUIActions $ uiActions browse

  srvAG <- actionGroupNew "server"
  actionGroupAddActions srvAG $ srvActions browse
  onServerConnectionAdd . ever $ actionGroupSetSensitive srvAG
  insertActionGroup srvAG 1

  addUIFromFile "collection-browser"

  context <- initListView
  let ?context = context

  paned <- hPanedNew
  boxPackStartDefaults contents paned

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll listView
  panedAdd1 paned scroll

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll collView
  panedAdd2 paned scroll

  return ()


uiActions browse =
  [ ActionEntry
    { actionEntryName        = "collection"
    , actionEntryLabel       = "_Collection"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "new-window"
    , actionEntryLabel       = "_New window"
    , actionEntryStockId     = Just stockNew
    , actionEntryAccelerator = Just "<Control>n"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = newWindow browse
    }
  ]

srvActions _browse = []

newWindow browse =
  browse Nothing
