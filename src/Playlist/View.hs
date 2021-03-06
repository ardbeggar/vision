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

{-# LANGUAGE TupleSections, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Playlist.View
  ( withView
  , playlistView
  , playlistSel
  , getSelectedTracks
  ) where

import Control.Applicative

import Data.Maybe
import Data.IORef

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Medialib
import Playback
import Builder
import Playlist.Model
import Playlist.Index
import Playlist.Format


data View
  = View { _view :: TreeView
         , _sel  :: TreeSelection
         , _cur  :: IORef (TreePath, Maybe TreeViewColumn)
         }

playlistView = _view ?_Playlist_View
playlistSel  = _sel ?_Playlist_View
playlistCur  = _cur ?_Playlist_View


newtype Wrap a = Wrap { unWrap :: (?_Playlist_View :: View) => a }

withView    = withView' . Wrap
withView' w = do
  view <- mkView
  let ?_Playlist_View = view

  treeViewSetModel playlistView playlistStore

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

  playlistView `on` cursorChanged $ do
    cur <- treeViewGetCursor playlistView
    writeIORef playlistCur cur

  playlistStore `on` rowDeleted $ \[d] -> do
    cur <- readIORef playlistCur
    case cur of
      ([n], c) | n >= d -> do
        s <- getPlaylistSize
        let n' = if n < s && n == d then n else max (n - 1) 0
            c' = maybe Nothing (Just . (, False)) c
        treeViewSetCursor playlistView [n'] c'
      _ -> return ()

  unWrap w


getInfoIfNeeded iter = do
  [n] <- treeModelGetPath playlistStore iter
  mid <- listStoreGetValue playlistStore n
  rng <- treeViewGetVisibleRange playlistView
  getInfo mid $ case rng of
    ([f], [t]) | n >= f && t >= n -> Visible
    _                             -> Background


mkView = do
  view <- getObject castToTreeView "playlist-view"
  sel  <- treeViewGetSelection view
  cur  <- newIORef ([], Nothing)
  return View { _view = view
              , _sel  = sel
              , _cur  = cur
              }


getSelectedTracks =
  map head <$> treeSelectionGetSelectedRows playlistSel