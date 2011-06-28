-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 26 Jun. 2010
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

{-# LANGUAGE NoMonoLocalBinds #-}

module Playlist.Edit
  ( editDelete
  , editCopy
  , editPaste
  , editSelectAll
  , editInvertSelection
  , editCheckClipboard
  ) where

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import Atoms
import Utils
import Clipboard
import Playback
import Playlist.Model
import Playlist.View
import Playlist.Control


editDelete cut = do
  tracks <- liftIO $ getSelectedTracks
  when cut $ copyIds tracks
  liftIO $ do
    removeTracks tracks
    maybeTrack <- currentTrackThisPlaylist
    withJust maybeTrack $ \t ->
      when (t `elem` tracks) restartPlayback

editCopy = copyIds =<< liftIO getSelectedTracks

editPaste append = do
  clipboard <- clipboard
  targets   <- getClipboardTargets
  liftIO $ paste clipboard targets
  where paste clipboard targets
          | xmms2MlibIdTarget `elem` targets =
            p clipboard xmms2MlibIdTarget (selectionDataGet selectionTypeInteger) insertIds
          | uriListTarget `elem` targets =
            p clipboard uriListTarget selectionDataGetURIs insertURIs
          | stringTarget `elem` targets =
            p clipboard stringTarget selectionDataGetText $ insertURIs . lines
          | otherwise =
            return ()
        p clipboard target get put =
          clipboardRequestContents clipboard target $ do
            maybeContents <- get
            withJust maybeContents $ \contents -> liftIO $ do
              (path, _) <- treeViewGetCursor playlistView
              put contents $ case path of
                [n] | not append -> Just n
                _                -> Nothing

editSelectAll =
  treeSelectionSelectAll playlistSel

editInvertSelection = do
  rows <- treeSelectionGetSelectedRows playlistSel
  treeSelectionSelectAll playlistSel
  mapM_ (treeSelectionUnselectPath playlistSel) rows

editCheckClipboard = do
  targets <- getClipboardTargets
  return $
    elem xmms2MlibIdTarget targets ||
    elem uriListTarget targets ||
    elem stringTarget targets

copyIds tracks = do
  clipboard <- clipboard
  liftIO $ do
    ids <- playlistGetIds tracks
    clipboardSetWithData clipboard
      [(xmms2MlibIdTarget, 0)]
      (const $ selectionDataSet selectionTypeInteger ids)
      (return ())
    return ()

