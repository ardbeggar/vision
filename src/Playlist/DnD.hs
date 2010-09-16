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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Playlist.DnD
  ( setupDnD
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk

import XMMS2.Client

import Atoms
import XMMS
import Utils

import Playlist.Model
import Playlist.View
import Playlist.Control


setupDnD = do
  let view  = playlistView
      store = playlistStore

  targetList <- targetListNew
  targetListAdd targetList xmms2PosList [TargetSameWidget] 0

  dragSourceSet view [Button1] [ActionMove]
  dragSourceSetTargetList view targetList

  sel <- treeViewGetSelection view
  view `on` dragDataGet $ \_ _ _ -> do
    rows <- liftIO $ treeSelectionGetSelectedRows sel
    selectionDataSet selectionTypeInteger $ map head rows
    return ()

  targetList <- targetListNew
  targetListAdd targetList xmms2PosList [TargetSameWidget] 0
  targetListAdd targetList xmms2MlibId [TargetSameApp] 1
  targetListAddUriTargets targetList 2

  dragDestSet view [DestDefaultMotion, DestDefaultHighlight]
    [ActionMove, ActionCopy]
  dragDestSetTargetList view targetList

  dropRef <- newIORef False

  view `on` dragDrop $ \ctxt _ tstamp -> do
    maybeTarget <- dragDestFindTarget view ctxt (Just targetList)
    case maybeTarget of
      Just target -> do
        writeIORef dropRef True
        dragGetData view ctxt target tstamp
        return True
      Nothing ->
        return False

  view `on` dragDataReceived $ \ctxt (_, y) infoId tstamp -> do
    drop <- liftIO $ readIORef dropRef
    when drop $ do
      liftIO $ writeIORef dropRef False
      case infoId of
        0 -> do
          rows <- selectionDataGet selectionTypeInteger
          liftIO $ do
            fmaybeM_ rows $ \rows -> do
              base <- getTargetRow store view y pred
              moveTracks $ reorder base rows
            dragFinish ctxt True True tstamp
        1 -> do
          ids <- selectionDataGet selectionTypeInteger
          liftIO $ do
            fmaybeM_ ids $ \ids -> do
              name <- getPlaylistName
              base <- getTargetRow store view y id
              zipWithM_ (playlistInsertId xmms name) [base .. ] ids
            dragFinish ctxt True False tstamp
        2 -> do
          uris <- selectionDataGetURIs
          liftIO $ do
            fmaybeM_ uris $ \uris -> do
              base <- getTargetRow store view y id
              addURIs base uris
            dragFinish ctxt True False tstamp
        _ -> liftIO $ dragFinish ctxt False False tstamp

  view `on` dragDataDelete $ \_ ->
    treeSelectionUnselectAll sel

getTargetRow store view y f = do
  maybePos <- treeViewGetPathAtPos view (0, y)
  case maybePos of
    Just ([n], _, _) ->
      return n
    Nothing ->
      f <$> listStoreGetSize store

reorder = reorderDown 0
  where reorderDown _ _ [] = []
        reorderDown dec base rows@(r:rs)
          | r <= base = (r - dec, base) : reorderDown (dec + 1) base rs
          | otherwise = reorderUp (if dec /= 0 then base + 1 else base) rows
        reorderUp _ [] = []
        reorderUp base (r:rs)
          | r == base = reorderUp (base + 1) rs
          | otherwise = (r, base) : reorderUp (base + 1) rs
