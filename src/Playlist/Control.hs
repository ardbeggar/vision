-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 2 Jul. 2010
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

module Playlist.Control
  ( clearPlaylist
  , currentTrackThisPlaylist
  , removeTracks
  , insertIds
  , moveTracks
  , playTrack
  , showPropertyEditor
  , showPropertyExport
  ) where

import Control.Monad

import XMMS2.Client

import XMMS
import Playback
import qualified Properties as P
import Playlist.Model
import Playlist.View


clearPlaylist = do
  playlistClear xmms =<< getPlaylistName
  return ()

currentTrackThisPlaylist = do
  maybeName <- getPlaylistName
  maybeCPos <- getCurrentTrack
  return $ case (maybeName, maybeCPos) of
    (Just pname, Just (track, cname)) | pname == cname ->
      Just track
    _ ->
      Nothing

removeTracks tracks = do
  name <- getPlaylistName
  mapM_ (playlistRemoveEntry xmms name) $ reverse tracks

insertIds ids pos = do
  start <- case pos of
    Just n  -> return n
    Nothing -> getPlaylistSize
  name <- getPlaylistName
  zipWithM_ (playlistInsertId xmms name) [start .. ] ids

moveTracks tracks = do
  name <- getPlaylistName
  mapM_ (uncurry (playlistMoveEntry xmms name)) tracks

playTrack n = do
  playlistSetNext xmms $ fromIntegral n
  startPlayback True

showPropertyEditor = withSelectedIds P.showPropertyEditor

showPropertyExport = withSelectedIds P.showPropertyExport

withSelectedIds f =
  f =<< playlistGetIds =<< getSelectedTracks
