-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection
  ( initCollection
  , browseCollection
  ) where

import Graphics.UI.Gtk

import UI
import Collection.Common
import Collection.List
import Collection.Model
import Collection.View
import Collection.Control
import Collection.DnD
import Collection.UI


initCollection = do
  context <- initCommon
  let ?context = context

  context <- initList
  let ?context = context

  return ?context


browseCollection maybeName = do
  let f = browseCollection

  context <- initModel
  let ?context = context

  context <- initView
  let ?context = context

  context <- initUI
  let ?context = context

  context <- initCollectionUI f
  let ?context = context

  setupDnD

  widgetShowAll window
  case maybeName of
    Just name -> loadNamed name
    Nothing   -> widgetGrabFocus collFilter
