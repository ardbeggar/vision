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

module Playlist.Model
  ( initModel
  , clearModel
  , playlistStore
  , playlistName
  , getPlaylistName
  , setPlaylistName
  , playlistSize
  , modPlaylistSize
  , getPlaylistSize
  , touchPlaylist
  , playlistGetIds
  ) where

import Control.Concurrent.STM

import Data.Int

import Graphics.UI.Gtk

import Context


data Model
  = Model { mStore               :: ListStore Int32
          , mPlaylistName        :: TVar (Maybe String)
          , mPlaylistSize        :: TVar Int
          }

playlistStore = mStore context
playlistName  = mPlaylistName context
playlistSize  = mPlaylistSize context

getPlaylistName = readTVarIO playlistName
setPlaylistName = atomically . writeTVar playlistName

getPlaylistSize = listStoreGetSize playlistStore
modPlaylistSize op = do
  sz <- readTVar playlistSize
  writeTVar playlistSize $ op sz

touchPlaylist n = do
  Just iter <- treeModelGetIter playlistStore [n]
  treeModelRowChanged playlistStore [n] iter

playlistGetIds =
  mapM (listStoreGetValue playlistStore)


initModel = do
  context <- initContext
  let ?context = context

  return ?context

clearModel =
  listStoreClear playlistStore


initContext = do
  store               <- listStoreNewDND [] Nothing Nothing
  playlistName        <- newTVarIO Nothing
  playlistSize        <- newTVarIO 0
  return $ augmentContext
    Model { mStore               = store
          , mPlaylistName        = playlistName
          , mPlaylistSize        = playlistSize
          }
