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

module Location
  ( browseLocation
  ) where

import Graphics.UI.Gtk

import UI
import Location.Model
import Location.View
import Location.Control
import Location.UI


browseLocation maybeURL = do
  let f = browseLocation

  env <- initUI
  let ?env = env

  env <- initModel
  let ?env = env

  env <- initView
  let ?env = env

  setupUI f

  widgetShowAll window
  case maybeURL of
    Just url -> loadLocation $ Go url
    Nothing  -> openLocation



