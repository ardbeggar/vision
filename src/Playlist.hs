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

import Env
import UI


showPlaylist = do
  env <- initUI "playlist" testUICont
  let ?env = env

  putStrLn getEnv
  widgetShowAll window


testUICont _ = do
  window `onDestroy` mainQuit
  return $ augmentEnv "zopa"
