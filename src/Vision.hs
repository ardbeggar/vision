-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 11 Jun. 2010
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

module Main
  where

import Graphics.UI.Gtk

import Environment
import Properties
import XMMS
import Medialib
import Playback
import Volume
import Playtime
import Clipboard
import Location
import Playlist


main = do
  initGUI

  env <- initEnvironment
  let ?env = env

  env <- initProperties
  let ?env = env

  env <- initXMMS
  let ?env = env

  env <- initMedialib
  let ?env = env

  env <- initPlayback
  let ?env = env

  env <- initVolume
  let ?env = env

  env <- initPlaytime
  let ?env = env

  env <- initClipboard
  let ?env = env

  env <- initLocation
  let ?env = env

  env <- initPlaylist
  let ?env = env

  showPlaylist

  mainGUI
