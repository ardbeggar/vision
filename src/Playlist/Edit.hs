-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 26 Jun. 2010
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

module Playlist.Edit
  ( editDelete
  , editCopy
  , editPaste
  , editSelectAll
  , editInvertSelection
  , editCheckClipboard
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import XMMS2.Client

import Atoms
import XMMS
import Clipboard
import Playlist.Model
import Playlist.View


editDelete cut = do
  rows <- map head <$> treeSelectionGetSelectedRows playlistSel
  when cut $ do
    ids  <- playlistGetIds rows
    clipboardSetWithData clipboard [(xmms2MlibId, 0)] (copyIds ids) (return ())
    return ()
  name <- getPlaylistName
  mapM_ (playlistRemoveEntry xmms name) $ reverse rows

editCopy = do
  rows <- map head <$> treeSelectionGetSelectedRows playlistSel
  ids  <- playlistGetIds rows
  clipboardSetWithData clipboard [(xmms2MlibId, 0)] (copyIds ids) (return ())

editPaste append =
  clipboardRequestContents clipboard xmms2MlibId $ do
    maybeIds <- selectionDataGet selectionTypeInteger
    case maybeIds of
      Just ids -> liftIO $ do
        name      <- getPlaylistName
        (path, _) <- treeViewGetCursor playlistView
        start     <- case path of
          [n] | not append -> return n
          _                -> getPlaylistSize
        zipWithM_ (playlistInsertId xmms name) [start .. ] ids
      _        -> return ()

editSelectAll =
  treeSelectionSelectAll playlistSel

editInvertSelection = do
  rows <- treeSelectionGetSelectedRows playlistSel
  treeSelectionSelectAll playlistSel
  mapM_ (treeSelectionUnselectPath playlistSel) rows

editCheckClipboard =
  elem xmms2MlibId <$> getClipboardTargets

copyIds ids _ =
  selectionDataSet selectionTypeInteger ids
