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

module Playlist.Update
  ( setupUpdate
  ) where

import Control.Monad.Trans

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import XMMS
import Handler

import Playlist.Model


setupUpdate = do
  onConnected . add . ever . const $ requestPlaylist
  onDisconnected . add . ever . const $ clearModel

requestPlaylist =
  playlistListEntries xmms Nothing >>* handlePlaylist

handlePlaylist = do
  ids <- result
  liftIO $ do
    clearModel
    mapM_ (listStoreAppend playlistStore) ids
  return False
