-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 10 Sep. 2010
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

{-# LANGUAGE TupleSections #-}

module Collection.Order
  ( OrderDialog
  , makeOrderDialog
  , showOrderDialog
  ) where

import Control.Monad

import Graphics.UI.Gtk

import UI
import Compound
import Editor
import Properties
import Collection.Control


type OrderDialog = EditorDialog (PropertyView Bool)

showOrderDialog dialog =
  runEditorDialog dialog
  getOrder
  setOrder
  False window

makeOrderDialog =
  makeEditorDialog [(stockApply, ResponseApply)]
  makeOrderView $ \v -> do
    let outerw = outer v
    windowSetTitle outerw "Configure ordering"
    windowSetDefaultSize outerw 500 400

makeOrderView parent onState = do
  view <- makePropertyView (, False) parent onState
  let store = propertyViewStore view
      right = propertyViewRight view

  treeViewSetRulesHint right True

  column <- treeViewColumnNew
  treeViewColumnSetTitle column "Order"
  treeViewAppendColumn right column
  cell <- cellRendererComboNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \(_, dir) ->
    [ cellText := dirToString dir ]

  cmod <- listStoreNewDND [False, True] Nothing Nothing
  let clid = makeColumnIdString 0
  customStoreSetColumn cmod clid dirToString
  cell `set` [ cellTextEditable   := True
             , cellComboHasEntry  := False
             , cellComboTextModel := (cmod, clid) ]
  cell `on` edited $ \[n] str -> do
    (prop, dir) <- listStoreGetValue store n
    let dir' = stringToDir str
    unless (dir' == dir) $
      listStoreSetValue store n (prop, dir')

  return view

dirToString False = "Ascending"
dirToString True  = "Descending"

stringToDir = ("Descending" ==)
