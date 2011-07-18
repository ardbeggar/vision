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

{-# LANGUAGE RankNTypes #-}

module Properties.Editor.View
  ( withEditorView
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

view    = _view ?_Properties_Editor_View
valCol  = _valCol ?_Properties_Editor_View
valCell = _valCell ?_Properties_Editor_View

onPropertyEdited = valCell `on` edited

resetView = do
  treeViewSetCursor view [0] $ Just (valCol, False)
  widgetGrabFocus view

newtype Wrap a = Wrap { unWrap :: (?_Properties_Editor_View :: View) => a }

withEditorView    = withEditorView' . Wrap
withEditorView' w = do
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

  unWrap w

mkView = do
  view    <- treeViewNewWithModel store
  valCol  <- treeViewColumnNew
  valCell <- cellRendererTextNew
  return View { _view    = view
              , _valCol  = valCol
              , _valCell = valCell
              }
