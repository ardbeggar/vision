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

{-# LANGUAGE Rank2Types #-}

module Location.View
  ( withView
  , locationView
  , locationSel
  , locationEntry
  , locationComp
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.List
import Data.Char
import Data.Maybe

import Graphics.UI.Gtk

import Builder
import Environment

import Location.Model
import Location.PathComp


data View
  = View { _view  :: TreeView
         , _sel   :: TreeSelection
         , _entry :: Entry
         , _comp  :: PathComp
         }

locationView  = _view ?_Location_View
locationSel   = _sel ?_Location_View
locationEntry = _entry ?_Location_View
locationComp  = _comp ?_Location_View


newtype Wrap a = Wrap { unWrap :: (?_Location_View :: View) => a }

withView    = withView' . Wrap
withView' w = do
  v <- mkView
  let ?_Location_View = v

  treeViewSetModel locationView sortModel

  treeSelectionSetMode locationSel SelectionMultiple

  column <- treeViewColumnNew
  treeViewAppendColumn locationView column
  treeViewColumnSetTitle column "Name"
  treeViewColumnSetSortOrder column =<< getSortOrder
  treeViewColumnSetSortIndicator column True
  treeViewColumnSetClickable column True
  column `onColClicked` do
    order <- treeViewColumnGetSortOrder column
    let order' = case order of
          SortAscending  -> SortDescending
          SortDescending -> SortAscending
    treeViewColumnSetSortOrder column order'
    setSortOrder order'

  cell <- cellRendererPixbufNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributeFunc column cell sortModel $ \iter -> do
    item <- itemByIter iter
    cell `set` [ cellPixbufStockId :=
                 if iIsDir item
                 then stockDirectory
                 else stockFile ]

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell sortModel $ \iter -> do
    item <- itemByIter iter
    cell `set` [ cellText := iName item ]

  treeViewSetEnableSearch locationView True
  treeViewSetSearchEqualFunc locationView $ Just $ \str iter -> do
    item <- itemByIter iter
    return $ isInfixOf (map toLower str) (map toLower $ iName item)

  entrySetCompletion locationEntry $ pathComp locationComp

  locationEntry `onEditableChanged` do
    url <- entryGetText locationEntry
    updatePathComp locationComp url

  locationEntry `on` keyPressEvent $ tryEvent $ do
    []    <- eventModifier
    "Tab" <- eventKeyName
    liftIO $ do
      (url, modify, ofs) <- makeURL <$> entryGetText locationEntry
      when modify $ do
        pos <- editableGetPosition locationEntry
        entrySetText locationEntry url
        editableSetPosition locationEntry $ pos + ofs
      updatePathComp locationComp url
      entryCompletionInsertPrefix $ pathComp locationComp
      entryCompletionComplete $ pathComp locationComp

  unWrap w


mkView = do
  view  <- getObject castToTreeView "location-view"
  sel   <- treeViewGetSelection view
  entry <- getObject castToEntry "location-entry"
  comp  <- makePathComp
  return View { _view  = view
              , _sel   = sel
              , _entry = entry
              , _comp  = comp
              }

makeURL url
  | "://" `isInfixOf` url =
    (url, False, 0)
  | "~/" `isPrefixOf` url && isJust homeDir =
      let pfx = "file://" ++ fromJust homeDir
          len = length pfx + length url - 1
      in (pfx ++ tail url, True, len)
  | otherwise =
    ("file://" ++ url, True, 7)
