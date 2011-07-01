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

module Collection.UI
  ( setupUI
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Maybe

import System.IO.Unsafe

import Graphics.UI.Gtk hiding (add)

import UI
import XMMS
import Utils
import Properties
  ( showPropertyImport
  , showPropertyManager
  , makeOrderDialog
  , showOrderDialog )
import Compound
import Collection.View
import Collection.List.View
import Collection.Control


setupUI builder browse = do
  context <- initListView builder
  let ?context = context

  setupActions builder browse
  setupConnection builder

  collFilter `onEntryActivate` applyFilter
  collFilter `onIconPress` \icon ->
    case icon of
      PrimaryIcon   -> entrySetText collFilter ""
      SecondaryIcon -> applyFilter

  popup <- getWidget castToMenu "ui/collection-popup"
  setupTreeViewPopup collView popup

  onCollectionActivated loadSelected
  onCollectionListMidClick $ browseSelected browse
  onCollectionListCR $ browseSelected browse

  popup <- getWidget castToMenu "ui/list-popup"
  setupTreeViewPopup listView popup

  return ()

setupActions builder browse = do
  orderDialog <- unsafeInterleaveIO $ makeOrderDialog $ \v -> do
    let outerw = outer v
    windowSetTitle outerw "Configure ordering"
    windowSetDefaultSize outerw 500 400

  bindActions builder
    [ ("new-window"            , newWindow browse                             )
    , ("edit-filter"           , editFilter                                   )
    , ("all-media"             , allMedia                                     )
    , ("configure-columns"     , showViewConfigDialog                         )
    , ("configure-ordering"    , showOrderDialog orderDialog getOrder setOrder)
    , ("manage-properties"     , showPropertyManager                          )
    , ("browse-in-new-window"  , browseSelected browse                        )
    , ("save-collection"       , saveCollection                               )
    , ("rename-collection"     , renameCollection                             )
    , ("delete-collection"     , removeCollection                             )
    , ("add-to-playlist"       , addToPlaylist False                          )
    , ("replace-playlist"      , addToPlaylist True                           )
    , ("coll-add-to-playlist"  , collAddToPlaylist False                      )
    , ("coll-replace-playlist" , collAddToPlaylist True                       )
    , ("list-add-to-playlist"  , listAddToPlaylist False                      )
    , ("list-replace-playlist" , listAddToPlaylist True                       )
    , ("copy"                  , editCopy                                     )
    , ("select-all"            , editSelectAll                                )
    , ("invert-selection"      , editInvertSelection                          )
    , ("edit-properties"       , showPropertyEditor                           )
    , ("export-properties"     , showPropertyExport                           )
    , ("import-properties"     , showPropertyImport                           )
    ]

  acts <- actions builder
          [ "rename-collection"
          , "delete-collection"
          ]
  onCollectionSelectionChanged $ do
    en <- isJust <$> getSelectedCollection
    mapM_ (`actionSetSensitive` en) acts

  acts <- actions builder
          [ "add-to-playlist"
          , "replace-playlist"
          ]
  let updatePA = do
        list <- widgetGetIsFocus listView
        coll <- widgetGetIsFocus collView
        mapM_ (`actionSetSensitive` (list || coll)) acts
        return False
      setupPA w = do
        w `on` focusInEvent $ liftIO updatePA
        w `on` focusOutEvent $ liftIO updatePA
  setupPA listView
  setupPA collView

  acts <- actions builder
          [ "copy"
          , "edit-properties"
          , "export-properties"
          ]
  let updateEdit = do
        en <- (/= 0) <$> treeSelectionCountSelectedRows collSel
        mapM_ (`actionSetSensitive` en) acts
  collSel `onSelectionChanged` updateEdit

  flip timeoutAdd 0 $ do
    updatePA
    updateEdit
    updateWindowTitle
    return False

  return ()

setupConnection builder = do
  ag <- builderGetObject builder castToActionGroup "server-actions"
  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    actionGroupSetSensitive ag conn
    collFilter `set` [ secondaryIconSensitive := conn ]

  return ()


newWindow browse =
  browse Nothing
