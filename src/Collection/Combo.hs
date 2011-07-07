-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

module Collection.Combo
  ( mkModel
  , mkCombo
  , ComboItem (..)
  ) where

import Data.IORef

import Graphics.UI.Gtk

import Properties.Model
import Properties.Property

import Collection.Actions


data ComboItem
  = CIProp Property
  | CITracks
  | CISeparator

separator CISeparator = True
separator _           = False

mkModel = do
  props <- getProperties
  store <- listStoreNewDND (map CIProp props) Nothing Nothing
  listStoreAppend store CISeparator
  listStoreAppend store CITracks
  return store

mkCombo abRef ae cmod = do
  combo <- comboBoxNewWithModel cmod
  comboBoxSetRowSeparatorSource combo $ Just (cmod, separator)

  cell <- cellRendererTextNew
  cellLayoutPackStart combo cell True
  cellLayoutSetAttributes combo cell cmod $ \p ->
    case p of
      CITracks    -> [ cellText := "Tracks" ]
      CIProp p    -> [ cellText := propName p ]
      CISeparator -> []

  combo `on` setFocusChild $ \fc -> do
    maybe (return ()) (const $ writeIORef abRef emptyAB) fc
    aEnableSel ae False
    aEnableRen ae False
    aEnableDel ae False

  return combo
