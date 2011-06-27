-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jul. 2010
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Collection.Control
  ( loadSelected
  , loadNamed
  , loadCurrent
  , browseSelected
  , addToPlaylist
  , collAddToPlaylist
  , listAddToPlaylist
  , applyFilter
  , editFilter
  , allMedia
  , saveCollection
  , renameCollection
  , removeCollection
  , updateWindowTitle
  , editCopy
  , editSelectAll
  , editInvertSelection
  , showPropertyEditor
  , showPropertyExport
  , getOrder
  , setOrder
  , getSelectedIds
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Data.Maybe

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Utils
import UI
import Atoms
import Clipboard
import qualified Properties as P
import Collection.Common
import Collection.Model hiding (setOrder)
import qualified Collection.Model as Model
import Collection.View
import Collection.List.View


loadSelected = do
  maybeName <- getSelectedCollection
  case maybeName of
    Just name ->
      loadNamed name
    Nothing   -> do
      setCurColl universe ""
      loadCurrent

loadNamed name =
  collGet xmms name "Collections" >>* do
    coll <- result
    liftIO $ do
      setCurColl coll name
      loadCurrent

setOrder order = do
  Model.setOrder True order
  loaded <- getLoaded
  when loaded loadCurrent

loadCurrent =
  loadCurrent' `catch` \ParseError ->
    informUser MessageError "Invalid filter text"

loadCurrent' = do
  text <- trim <$> entryGetText collFilter
  entrySetText collFilter text
  setFilter text
  coll <- getCurColl
  keys <- getOrderKeys
  collQueryIds xmms coll keys 0 0 >>* do
    ids <- result
    liftIO $ do
      setLoaded True
      updateWindowTitle
      populateModel ids
      widgetGrabFocus collView

browseSelected browse =
  browse =<< getSelectedCollection


addToPlaylist replace = do
  list <- widgetGetIsFocus listView
  if list
    then listAddToPlaylist replace
    else do
    coll <- widgetGetIsFocus collView
    when coll $ collAddToPlaylist replace

collAddToPlaylist replace =
  add `catch` \ParseError ->
    informUser MessageError "Invalid filter text"
  where add = do
          ids <- getSelectedIds
          cur <- getCurColl
          sel <- collNewIdlist ids
          int <- collNew TypeIntersection
          collAddOperand int cur
          collAddOperand int sel
          addCollection replace int

listAddToPlaylist replace = do
  maybeName <- getSelectedCollection
  case maybeName of
    Nothing   -> addCollection replace universe
    Just name -> collGet xmms name "Collections" >>* do
      coll <- result
      liftIO $ addCollection replace coll

addCollection replace coll = do
  when replace $ playlistClear xmms Nothing >> return ()
  playlistAddCollection xmms Nothing coll =<< getOrderKeys
  return ()

getSelectedIds =
  mapM (listStoreGetValue collStore . head)
  =<< treeSelectionGetSelectedRows collSel

applyFilter = do
  conn <- connected
  when conn loadSelected

editFilter = do
  editableSelectRegion collFilter 0 (-1)
  widgetGrabFocus collFilter

allMedia = do
  resetModel
  resetListView
  entrySetText collFilter ""
  widgetGrabFocus collFilter
  updateWindowTitle

saveCollection = do
  name <- trim <$> getCurName
  coll <- getCurColl
  res  <- runDlg "Save collection" (not $ null name) (const True) name
  withJust res $ \name -> do
    collSave xmms coll name "Collections"
    return ()

renameCollection = do
  old <- fromJust <$> getSelectedCollection
  res <- runDlg "Rename collection" False (/= old) old
  withJust res $ \new -> do
    collRename xmms old new "Collections"
    return ()

removeCollection = do
  name <- fromJust <$> getSelectedCollection
  collRemove xmms name "Collections"
  return ()

runDlg title enable isOk init = do
  dialog <- dialogNew
  windowSetTitle dialog title
  windowSetTransientFor dialog window
  windowSetModal dialog True
  windowGroupAddWindow windowGroup dialog

  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk
  dialogSetDefaultResponse dialog ResponseOk
  dialogSetResponseSensitive dialog ResponseOk enable

  box <- vBoxNew False 0
  containerSetBorderWidth box 7
  upper <- dialogGetUpper dialog
  containerAdd upper box

  entry <- entryNew
  entrySetText entry init
  editableSelectRegion entry 0 (-1)
  editableSetPosition entry (-1)
  boxPackStart box entry PackNatural 0

  let ok = do
        new <- trim <$> entryGetText entry
        return $ not (null new) && isOk new
      check = dialogSetResponseSensitive dialog ResponseOk =<< ok
      checkInsert str pos = check >> return (length str + pos)
      checkDelete _ _     = check
  entry `onEntryActivate` do
    ok <- ok
    when ok $ dialogResponse dialog ResponseOk
  entry `afterInsertText` checkInsert
  entry `afterDeleteText` checkDelete

  widgetShowAll dialog
  resp <- dialogRun dialog
  new  <- trim <$> entryGetText entry
  widgetDestroy dialog

  return $ case resp of
    ResponseOk -> Just new
    _          -> Nothing

updateWindowTitle = do
  name   <- getCurName
  loaded <- getLoaded
  filter <- getFilter
  let t = if loaded then f ++ n ++ " - " else ""
      n = case name of
        "" -> "All media"
        _  -> name
      f = if null filter then "" else "*"
  setWindowTitle $ t ++ "Vision collection browser"

editCopy = do
  return ()
{-
  ids <- getSelectedIds
  clipboardSetWithData clipboard
    [(xmms2MlibIdTarget, 0)]
    (const $ selectionDataSet selectionTypeInteger ids)
    (return ())
  return ()
-}

editSelectAll =
  treeSelectionSelectAll collSel

editInvertSelection = do
  rows <- treeSelectionGetSelectedRows collSel
  treeSelectionSelectAll collSel
  mapM_ (treeSelectionUnselectPath collSel) rows


showPropertyEditor = withSelectedIds P.showPropertyEditor

showPropertyExport = withSelectedIds P.showPropertyExport

withSelectedIds f =
  f =<< getSelectedIds

getOrderKeys =
  P.encodeOrder <$> getOrder
