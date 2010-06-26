-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

module Playlist
  ( showPlaylist
  ) where

import Graphics.UI.Gtk

import UI

import Playlist.Model
import Playlist.Index
import Playlist.Format
import Playlist.View
import Playlist.Search
import Playlist.Update
import Playlist.DnD
import Playlist.UI


showPlaylist = do
  env <- initUI
  let ?env = env

  env <- initModel
  let ?env = env

  env <- initFormat
  let ?env = env

  env <- initIndex
  let ?env = env

  env <- initView
  let ?env = env

  setupSearch
  setupUpdate
  setupDnD
  setupUI

  widgetShowAll window
