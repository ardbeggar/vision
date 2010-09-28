-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Jun. 2010
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

module Playlist.DnD
  ( setupDnD
  ) where

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import XMMS2.Client

import Atoms
import XMMS
import Utils
import DnD

import Playlist.Model
import Playlist.View
import Playlist.Control


setupDnD = do
  let view  = playlistView
      store = playlistStore

  targetList <- targetListNew
  targetListAdd targetList xmms2PosListTarget [TargetSameWidget] 0

  dragSourceSet view [Button1] [ActionMove]
  dragSourceSetTargetList view targetList

  sel <- treeViewGetSelection view
  view `on` dragDataGet $ \_ _ _ -> do
    rows <- liftIO $ treeSelectionGetSelectedRows sel
    selectionDataSet selectionTypeInteger $ map head rows
    return ()

  setupDest view
    [DestDefaultMotion, DestDefaultHighlight]
    [ActionMove, ActionCopy]
    [ xmms2PosListTarget :>: \(_, y) -> do
         rows <- selectionDataGet selectionTypeInteger
         liftIO $ withJust rows $ \rows -> do
           base <- getTargetRow store view y True
           moveTracks $ reorder base rows
         return (True, True)
    , xmms2MlibIdTarget :>: \(_, y) -> do
         ids <- selectionDataGet selectionTypeInteger
         liftIO $ withJust ids $ \ids -> do
           name <- getPlaylistName
           base <- getTargetRow store view y False
           zipWithM_ (playlistInsertId xmms name) [base .. ] ids
         return (True, False)
    , URITargets :>: \(_, y) -> do
         uris <- selectionDataGetURIs
         liftIO $ withJust uris $ \uris -> do
           base <- getTargetRow store view y False
           insertURIs uris $ Just base
         return (True, False)
    ]

  view `on` dragDataDelete $ \_ ->
    treeSelectionUnselectAll sel
