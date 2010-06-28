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

module Playlist.View
  ( initView
  , playlistView
  , playlistSel
  , updateWindowTitle
  , getSelectedTracks
  ) where

import Control.Applicative

import Data.Maybe

import Graphics.UI.Gtk

import XMMS2.Client

import Env
import UI
import Playback
import XMMS
import Playlist.Model
import Playlist.Index
import Playlist.Format


data View
  = View { vView :: TreeView
         , vSel  :: TreeSelection }

playlistView = vView getEnv
playlistSel  = vSel getEnv


initView = do
  env <- initEnv
  let ?env = env

  treeViewSetRulesHint playlistView True
  treeViewSetReorderable playlistView True
  treeViewSetHeadersVisible playlistView False

  treeSelectionSetMode playlistSel SelectionMultiple

  column <- treeViewColumnNew
  treeViewInsertColumn playlistView column 0

  cell <- cellRendererPixbufNew
  cell `set` [ cellWidth := 30 ]
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributeFunc column cell playlistStore $ \iter -> do
    [n] <- treeModelGetPath playlistStore iter
    maybeCT <- getCurrentTrack
    name <- fromMaybe "" <$> getPlaylistName
    cell `set` [ cellPixbufStockId :=>
                 case maybeCT of
                   Just (cp, cname) | cp == n && cname == name -> do
                     maybeStatus <- getPlaybackStatus
                     case maybeStatus of
                       Just StatusPlay  -> return stockMediaPlay
                       Just StatusPause -> return stockMediaPause
                       Just StatusStop  -> return stockMediaStop
                       _                -> return ""
                   _ ->
                     return ""
               ]

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell playlistStore $ \iter -> do
    info <- getInfoIfNeeded iter
    cell `set` trackInfoAttrs info

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributeFunc column cell playlistStore $ \iter -> do
    info <- getInfoIfNeeded iter
    cell `set` [ cellText := trackInfoDuration info ]

  playlistView `onRowActivated` \[n] _ -> do
    playlistSetNext xmms $ fromIntegral n
    startPlayback True
    return ()

  return ?env


getInfoIfNeeded iter = do
  [n] <- treeModelGetPath playlistStore iter
  mid <- listStoreGetValue playlistStore n
  rng <- treeViewGetVisibleRange playlistView
  getInfo mid $ case rng of
    ([f], [t]) -> n >= f && t >= n
    _          -> False


updateWindowTitle = do
  maybeName <- getPlaylistName
  setWindowTitle $ case maybeName of
    Nothing   -> "Vision playlist"
    Just name -> name ++ " - Vision playlist"

initEnv = do
  view <- treeViewNewWithModel playlistStore
  sel  <- treeViewGetSelection view
  return $ augmentEnv
    View { vView = view
         , vSel  = sel }


getSelectedTracks =
  map head <$> treeSelectionGetSelectedRows playlistSel