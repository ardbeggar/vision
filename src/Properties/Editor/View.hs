-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

module Properties.Editor.View
  ( WithView
  , withView
  , view
  , resetView
  , onPropertyEdited
  ) where

import Graphics.UI.Gtk

import XMMS
import Properties.Property
import Properties.Editor.Model


data View
  = View { _view    :: TreeView
         , _valCol  :: TreeViewColumn
         , _valCell :: CellRendererText
         }

type WithView = ?_Properties_Editor_View :: View

view :: WithView => TreeView
view = _view ?_Properties_Editor_View

valCol :: WithView => TreeViewColumn
valCol = _valCol ?_Properties_Editor_View

valCell :: WithView => CellRendererText
valCell = _valCell ?_Properties_Editor_View

onPropertyEdited :: WithView => (TreePath -> String -> IO ()) -> IO (ConnectId CellRendererText)
onPropertyEdited = valCell `on` edited

resetView :: WithView => IO ()
resetView = do
  treeViewSetCursor view [0] $ Just (valCol, False)
  widgetGrabFocus view

withView :: (WithXMMS, WithModel) => (WithView => IO a) -> IO a
withView func = do
  ev <- mkView
  let ?_Properties_Editor_View = ev

  treeViewSetRulesHint view True
  treeViewSetHeadersVisible view False
  treeViewSetReorderable view True

  column <- treeViewColumnNew
  treeViewAppendColumn view column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \prop ->
    [ cellText       := propName prop
    , cellTextWeight := 800
    ]

  treeViewAppendColumn view valCol
  treeViewColumnPackStart valCol valCell True
  cellLayoutSetAttributes valCol valCell store $ \prop ->
    [ cellText         :=> propertyText prop
    , cellTextEditable :=> do
      c <- connected
      return $ c && not (propReadOnly prop)
    ]

  func

mkView :: WithModel => IO View
mkView = do
  view    <- treeViewNewWithModel store
  valCol  <- treeViewColumnNew
  valCell <- cellRendererTextNew
  return View { _view    = view
              , _valCol  = valCol
              , _valCell = valCell
              }
