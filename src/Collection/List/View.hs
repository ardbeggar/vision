-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 10 Mar. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module Collection.List.View
  ( initListView
  , listView
  , resetListView
  , onCollectionActivated
  , onCollectionSelectionChanged
  , onCollectionListMidClick
  , onCollectionListCR
  , getSelectedCollection
  ) where

import Control.Applicative
import Control.Monad.Trans

import Data.Char (toLower)
import Data.List (isInfixOf)

import Graphics.UI.Gtk

import Context
import Collection.List.Model


data View
  = View { vView :: TreeView
         , vSel  :: TreeSelection
         }

listView = vView context
listSel  = vSel  context


initListView builder = do
  context <- initContext builder
  let ?context = context

  treeSelectionSetMode listSel SelectionBrowse

  treeViewSetModel listView listStore

  treeViewSetHeadersVisible listView False
  widgetSetSizeRequest listView 200 (-1)

  column <- treeViewColumnNew
  treeViewAppendColumn listView column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell listStore $ \n ->
    case n of
      Nothing ->
        [ cellText := "All Media", cellTextWeight := 800 ]
      Just cn ->
        [ cellText := cn, cellTextWeightSet := False ]

  treeViewSetEnableSearch listView True
  treeViewSetSearchEqualFunc listView . Just $ \str iter ->
    maybe False (isInfixOf (map toLower str) . map toLower) <$>
      (listStoreGetValue listStore . head
       =<< treeModelGetPath listStore iter)

  return ?context

resetListView =
  treeViewSetCursor listView [0] Nothing


initContext builder = do
  view <- builderGetObject builder castToTreeView "list-view"
  sel  <- treeViewGetSelection view
  return $ augmentContext
    View { vView = view
         , vSel  = sel
         }

onCollectionActivated =
  onRowActivated listView . const . const

onCollectionSelectionChanged =
  onSelectionChanged listSel

onCollectionListMidClick f =
  listView `on` buttonPressEvent $ tryEvent $ do
    MiddleButton <- eventButton
    SingleClick  <- eventClick
    (x, y)       <- eventCoordinates
    liftIO $ do
      Just (p, _, _) <- treeViewGetPathAtPos listView (round x, round y)
      treeViewSetCursor listView p Nothing
      f

onCollectionListCR f =
  listView `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "Return"  <- eventKeyName
    liftIO f

getSelectedCollection = do
  path <- fst <$> treeViewGetCursor listView
  case path of
    [n] -> listStoreGetValue listStore n
    _   -> return Nothing

