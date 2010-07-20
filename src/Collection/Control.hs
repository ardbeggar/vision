-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jul. 2010
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

module Collection.Control
  ( loadSelected
  , loadNamed
  , loadCurrent
  , browseSelected
  , addToPlaylist
  , replacePlaylist
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO

import Control.Monad.Trans

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Collection.Common
import Collection.Model
import Collection.View
import Collection.List.View


loadSelected = do
  maybeName <- getSelectedCollection
  case maybeName of
    Just name ->
      loadNamed name
    Nothing   -> do
      setCurColl universe
      loadCurrent

loadNamed name =
  collGet xmms name "Collections" >>* do
    coll <- result
    liftIO $ do
      setCurColl coll
      loadCurrent
    return False

loadCurrent = loadCurrent' `catch` \ParseError -> return ()

loadCurrent' = do
  coll <- getCurColl
  collQueryIds xmms coll [] 0 0 >>* do
    ids <- result
    liftIO $ do
      populateModel ids
      widgetGrabFocus collView
    return False

browseSelected browse =
  browse =<< getSelectedCollection


addToPlaylist = addToPlaylist' `catch` \ParseError -> return ()

addToPlaylist' = do
  ids  <- getSelectedIds
  coll <- case ids of
    [] -> getCurColl
    _  -> do
      cur <- getCurColl
      sel <- collNewIdlist ids
      int <- collNew TypeIntersection
      collAddOperand int cur
      collAddOperand int sel
      return int
  playlistAddCollection xmms Nothing coll []
  return ()

replacePlaylist = do
  playlistClear xmms Nothing
  addToPlaylist

getSelectedIds =
  mapM (listStoreGetValue collStore . head)
  =<< treeSelectionGetSelectedRows collSel