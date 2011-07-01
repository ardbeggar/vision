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
import Utils
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

  ctW <- atomically $ newEmptyTWatch currentTrack
  let monCT old = do
        new <- atomically $ watch ctW
        postGUISync $ do
          name <- fromMaybe "" <$> getPlaylistName
          size <- getPlaylistSize
          withJust old $ \(ot, oname) ->
            when (oname == name && ot < size) $
              touchPlaylist ot
          withJust new $ \(nt, nname) ->
            when (nname == name && nt < size) $
              touchPlaylist nt
        monCT new
  forkIO $ monCT Nothing

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
  len <- resultLength
  liftIO $ do
    clearModel
    mapM_ addToPlaylist ids
    atomically $ modPlaylistSize $ const $ fromIntegral len
    requestCurrentTrack

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
          atomically $ modPlaylistSize pred
        PlaylistAdd { mlibId = id } -> do
          n <- listStoreAppend playlistStore id
          addToIndex id n
          atomically $ modPlaylistSize succ
        PlaylistInsert { mlibId = id, position = n } -> do
          listStoreInsert playlistStore n id
          addToIndex id n
          atomically $ modPlaylistSize succ
        PlaylistMove { mlibId = id, position = o, newPosition = n } -> do
          listStoreRemove playlistStore o
          listStoreInsert playlistStore n id
          addToIndex id n
        PlaylistClear {} -> do
          clearModel
          atomically $ modPlaylistSize $ const 0
        _ ->
          requestPlaylist name
  persist
