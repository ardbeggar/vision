-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 23 Jun. 2010
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

module Playlist.Search
  ( setupSearch
  ) where

import Data.List
import Data.Char

import Graphics.UI.Gtk

import Playlist.Model
import Playlist.Index
import Playlist.View
import Playlist.Format


setupSearch = do
  treeViewSetEnableSearch playlistView True
  treeViewSetSearchEqualFunc playlistView $ Just $ \str iter -> do
    [n]  <- treeModelGetPath playlistStore iter
    mid  <- listStoreGetValue playlistStore n
    info <- getInfo mid True
    return $ isInfixOf (map toLower str) (trackInfoText info)
