-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Jul. 2011
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

module Collection.ComboModel
  ( mkModel
  , ComboItem (..)
  ) where

import Graphics.UI.Gtk

import Properties.Model
import Properties.Property


data ComboItem
  = CIProp Property
  | CITracks
  | CISeparator

mkModel :: WithModel => IO (ListStore ComboItem)
mkModel = do
  props <- getProperties
  store <- listStoreNewDND (map CIProp props) Nothing Nothing
  listStoreAppend store CISeparator
  listStoreAppend store CITracks
  return store
