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
  ( initCollectionUI
  ) where

import Control.Applicative
import Control.Monad.Trans

import Data.Maybe

import Graphics.UI.Gtk hiding (add)

import UI
import XMMS
import Handler
import Utils
import Collection.View
import Collection.List.View
import Collection.Control


initCollectionUI browse = do
  context <- initListView
  let ?context = context

  addUIActions $ uiActions browse

  srvAG <- actionGroupNew "server"
  actionGroupAddActions srvAG $ srvActions browse
  onServerConnectionAdd . ever $ actionGroupSetSensitive srvAG
  insertActionGroup srvAG 1

  addUIFromFile "collection-browser"

  onCollectionActivated $ loadSelected
  onCollectionListMidClick $ browseSelected browse
  onCollectionListCR $ browseSelected browse

  paned <- hPanedNew
  boxPackStartDefaults contents paned

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll listView
  panedAdd1 paned scroll

  box <- vBoxNew False 5
  panedAdd2 paned box

  boxPackStart box collFilter PackNatural 0
  collFilter `onEntryActivate` applyFilter

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll collView
  boxPackStartDefaults box scroll

  popup <- getWidget castToMenu "ui/collection-popup"
  setupTreeViewPopup collView popup

  popup <- getWidget castToMenu "ui/list-popup"
  setupTreeViewPopup listView popup

  acts <- mapM (getAction srvAG)
          [ "rename-collection"
          , "remove-collection"
          ]
  onCollectionSelectionChanged $ do
    en <- isJust <$> getSelectedCollection
    mapM_ (flip actionSetSensitive en) acts

  acts <- mapM (getAction srvAG)
          [ "add-to-playlist"
          , "replace-playlist"
          ]
  let updatePA = liftIO $ do
        list <- widgetGetIsFocus listView
        coll <- widgetGetIsFocus collView
        mapM_ (flip actionSetSensitive (list || coll)) acts
        return False
      setupPA w = do
        w `on` focusInEvent $ updatePA
        w `on` focusOutEvent $ updatePA
  setupPA listView
  setupPA collView
  updatePA

  acts <- mapM (getAction srvAG) ["copy"]
  let updateE = do
        en <- (/= 0) <$> treeSelectionCountSelectedRows collSel
        mapM_ (flip actionSetSensitive en) acts
  collSel `onSelectionChanged` updateE
  updateE

  updateWindowTitle
  return ?context

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
  , ActionEntry
    { actionEntryName        = "edit-filter"
    , actionEntryLabel       = "_Edit filter"
    , actionEntryStockId     = Just stockEdit
    , actionEntryAccelerator = Just "<Control>l"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editFilter
    }
  , ActionEntry
    { actionEntryName        = "all-media"
    , actionEntryLabel       = "A_ll media"
    , actionEntryStockId     = Just stockNew
    , actionEntryAccelerator = Just "<Control>u"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = allMedia
    }
  , ActionEntry
    { actionEntryName        = "edit"
    , actionEntryLabel       = "_Edit"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "collection-popup"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  ]

srvActions browse =
  [ ActionEntry
    { actionEntryName        = "browse-in-new-window"
    , actionEntryLabel       = "_Browse in new window"
    , actionEntryStockId     = Just stockOpen
    , actionEntryAccelerator = Just "<Control>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = browseSelected browse
    }
  , ActionEntry
    { actionEntryName        = "save-collection"
    , actionEntryLabel       = "_Save collection"
    , actionEntryStockId     = Just stockSave
    , actionEntryAccelerator = Just "<Control>s"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = saveCollection
    }
  , ActionEntry
    { actionEntryName        = "rename-collection"
    , actionEntryLabel       = "Rena_me collection"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = renameCollection
    }
  , ActionEntry
    { actionEntryName        = "remove-collection"
    , actionEntryLabel       = "Remo_ve collection"
    , actionEntryStockId     = Just stockDelete
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = removeCollection
    }
  , ActionEntry
    { actionEntryName        = "add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Just "<Control>p"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = addToPlaylist False
    }
  , ActionEntry
    { actionEntryName        = "replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Just "<Control><Shift>p"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = addToPlaylist True
    }
  , ActionEntry
    { actionEntryName        = "coll-add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = collAddToPlaylist False
    }
  , ActionEntry
    { actionEntryName        = "coll-replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = collAddToPlaylist True
    }
  , ActionEntry
    { actionEntryName        = "list-add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = listAddToPlaylist False
    }
  , ActionEntry
    { actionEntryName        = "list-replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = listAddToPlaylist True
    }
  , ActionEntry
    { actionEntryName        = "copy"
    , actionEntryLabel       = "_Copy"
    , actionEntryStockId     = Just stockCopy
    , actionEntryAccelerator = Just "<Control>c"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editCopy
    }
  , ActionEntry
    { actionEntryName        = "select-all"
    , actionEntryLabel       = "_Select all"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editSelectAll
    }
  , ActionEntry
    { actionEntryName        = "invert-selection"
    , actionEntryLabel       = "_Invert selection"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control><Shift>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editInvertSelection
    }
  ]


newWindow browse =
  browse Nothing