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
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.Order
  ( OrderDialog
  , makeOrderDialog
  , showOrderDialog
  , encodeOrder
  ) where

import Control.Monad

import Graphics.UI.Gtk

import UI
import Editor
import Properties.Property
import Properties.Model
import Properties.View


type OrderDialog = EditorDialog (PropertyView Bool)

showOrderDialog ::
  WithUI
  => OrderDialog
  -> IO [(Property, Bool)]
  -> ([(Property, Bool)] -> IO ())
  -> IO ()
showOrderDialog dialog getOrder setOrder =
  runEditorDialog dialog getOrder setOrder False window

makeOrderDialog :: WithModel => (OrderDialog -> IO a) -> IO OrderDialog
makeOrderDialog =
  makeEditorDialog [(stockApply, ResponseApply)] makeOrderView

makeOrderView :: WithModel => t -> IO () -> IO (PropertyView Bool)
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

dirToString :: Bool -> String
dirToString False = "Ascending"
dirToString True  = "Descending"

stringToDir :: String -> Bool
stringToDir = ("Descending" ==)

encodeOrder :: [(Property, Bool)] -> [String]
encodeOrder = map enc
  where enc (prop, False) = propKey prop
        enc (prop, True)  = '-' : propKey prop
