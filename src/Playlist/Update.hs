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
  ( initUpdate
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client hiding (playbackStatus)

import XMMS
import Handler
import Playback
import Playlist.Model
import Playlist.Index


initUpdate = do
  onServerConnectionAdd . ever $ \conn ->
    if conn
    then do
      playlistCurrentActive xmms >>* do
        setupPlaylist
        liftIO $ broadcastPlaylistChanged xmms >>* handleChange
      broadcastPlaylistLoaded xmms >>* do
        setupPlaylist
        persist
    else do
      setPlaylistName Nothing
      clearModel

  psW <- atomically $ newTWatch playbackStatus Nothing
  forkIO $ forever $ do
    void $ atomically $ watch psW
    postGUISync $ do
      maybeCT <- getCurrentTrack
      name    <- fromMaybe "" <$> getPlaylistName
      size    <- getPlaylistSize
      case maybeCT of
        Just (ct, cname) | cname == name && ct < size ->
          touchPlaylist ct
        _ ->
          return ()

  onCurrentTrack . add . ever $ \old -> do
    name <- fromMaybe "" <$> getPlaylistName
    size <- getPlaylistSize
    case old of
      Just (ot, oname) | oname == name && ot < size ->
        touchPlaylist ot
      _ ->
        return ()
    new <- getCurrentTrack
    case new of
      Just (nt, nname) | nname == name && nt < size ->
        touchPlaylist nt
      _ ->
        return ()

  return ?context

setupPlaylist = do
  name <- result
  liftIO $ do
    setPlaylistName $ Just name
    requestPlaylist name

requestPlaylist name =
  playlistListEntries xmms (Just name) >>* handlePlaylist

handlePlaylist = do
  ids <- result
  liftIO $ do
    clearModel
    mapM_ addToPlaylist ids
    requestCurrentTrack
    onPlaylistUpdated $ invoke ()

addToPlaylist id = do
  n <- listStoreAppend playlistStore id
  addToIndex id n

handleChange = do
  change <- result
  liftIO $ do
    name <- fromMaybe "" <$> getPlaylistName
    when (name == playlist change) $
      case change of
        PlaylistRemove { position = p } -> do
          listStoreRemove playlistStore p
          onPlaylistUpdated $ invoke ()
        PlaylistAdd { mlibId = id } -> do
          n <- listStoreAppend playlistStore id
          addToIndex id n
          onPlaylistUpdated $ invoke ()
        PlaylistInsert { mlibId = id, position = n } -> do
          listStoreInsert playlistStore n id
          addToIndex id n
          onPlaylistUpdated $ invoke ()
        PlaylistMove { mlibId = id, position = o, newPosition = n } -> do
          listStoreRemove playlistStore o
          listStoreInsert playlistStore n id
          addToIndex id n
          onPlaylistUpdated $ invoke ()
        PlaylistClear {} -> do
          clearModel
          onPlaylistUpdated $ invoke ()
        _ ->
          requestPlaylist name
  persist
