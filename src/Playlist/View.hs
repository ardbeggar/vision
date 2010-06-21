-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

module Playlist.View
  ( initView
  , playlistView
  , updateWindowTitle
  ) where

import Graphics.UI.Gtk

import Env
import UI

import Playlist.Model


data View
  = View { vView :: TreeView }

playlistView  = vView getEnv


initView env builder = do
  env <- initEnv env builder
  let ?env = env

  window `onDestroy` mainQuit

  treeViewSetModel playlistView playlistStore

  sel <- treeViewGetSelection playlistView
  treeSelectionSetMode sel SelectionMultiple

  column <- treeViewColumnNew
  treeViewInsertColumn playlistView column 0

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell playlistStore $ \iter -> do
    [n] <- treeModelGetPath playlistStore iter
    mid <- listStoreGetValue playlistStore n
    cell `set` [ cellText := show mid ]

  return ?env


updateWindowTitle = do
  maybeName <- getPlaylistName
  case maybeName of
    Nothing   ->
      setWindowTitle "Playlist - Vision"
    Just name ->
      setWindowTitle $ "Playlist: " ++ name ++ " - Vision"

initEnv env builder = do
  let ?env = env
  view <- builderGetObject builder castToTreeView "playlist-view"
  return $ augmentEnv
    View { vView = view }
