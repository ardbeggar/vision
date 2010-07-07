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

module Properties.Editor.View
  ( initEditorView
  , view
  , resetView
  , onPropertyEdited
  ) where

import Graphics.UI.Gtk

import Context
import Properties.Property
import Properties.Editor.Model


data View
  = View { vView    :: TreeView
         , vValCol  :: TreeViewColumn
         , vValCell :: CellRendererText
         }

view    = vView context
valCol  = vValCol context
valCell = vValCell context

onPropertyEdited = valCell `on` edited

resetView = do
  treeViewSetCursor view [0] $ Just (valCol, False)
  widgetGrabFocus view


initEditorView = do
  context <- initContext
  let ?context = context

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
    , cellTextEditable :=  not $ propReadOnly prop
    ]

  return ?context

initContext = do
  view    <- treeViewNewWithModel store
  valCol  <- treeViewColumnNew
  valCell <- cellRendererTextNew
  return $ augmentContext
    View { vView    = view
         , vValCol  = valCol
         , vValCell = valCell
         }
