-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

{-# LANGUAGE Rank2Types #-}

module Playlist.Model
  ( withModel
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


data Model
  = Model { _store        :: ListStore Int32
          , _playlistName :: TVar (Maybe String)
          , _playlistSize :: TVar Int
          }

playlistStore = _store ?_Playlist_Model
playlistName  = _playlistName ?_Playlist_Model
playlistSize  = _playlistSize ?_Playlist_Model

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


newtype Wrap a = Wrap { unWrap :: (?_Playlist_Model :: Model) => a }

withModel    = withModel' . Wrap
withModel' w = do
  model <- mkModel
  let ?_Playlist_Model = model in unWrap w

clearModel =
  listStoreClear playlistStore

mkModel = do
  store        <- listStoreNewDND [] Nothing Nothing
  playlistName <- newTVarIO Nothing
  playlistSize <- newTVarIO 0
  return Model { _store        = store
               , _playlistName = playlistName
               , _playlistSize = playlistSize
               }
