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

{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification #-}

module Playlist.DnD
  ( setupDnD
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef
import qualified Data.IntMap as IntMap

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


data CommonTargets
  = URITargets

class TargetClass t where
  addToTargetList :: TargetList -> InfoId -> t -> IO ()

instance TargetClass TargetTag where
  addToTargetList tl id tg = targetListAdd tl tg [] id

instance TargetClass CommonTargets where
  addToTargetList tl id tg =
    case tg of
      URITargets -> targetListAddUriTargets tl id

data Dest
  = forall t. TargetClass t => t :|: Dest
  | forall t. TargetClass t => t :>: (Point -> SelectionDataM (Bool, Bool))

infixr 9 :|:, :>:


setupDest widget defs acts dests = do
  tl <- targetListNew
  hm <- IntMap.fromList <$> zipWithM (setupDest' tl) [0 .. ] dests

  dragDestSet widget defs acts
  dragDestSetTargetList widget tl

  dropRef <- newIORef False

  widget `on` dragDrop $ \ctxt _ tstamp -> do
    maybeTarget <- dragDestFindTarget widget ctxt (Just tl)
    case maybeTarget of
      Just target -> do
        writeIORef dropRef True
        dragGetData widget ctxt target tstamp
        return True
      Nothing ->
        return False

  widget `on` dragDataReceived $ \ctxt pos infoId tstamp -> do
    drop <- liftIO $ readIORef dropRef
    when drop $ do
      liftIO $ writeIORef dropRef False
      (ok, del) <- case IntMap.lookup (fromIntegral infoId) hm of
        Just handler -> handler pos
        Nothing      -> return (False, False)
      liftIO $ dragFinish ctxt ok del tstamp

  return ()

setupDest' tl id (t :|: rest) = do
  addToTargetList tl id t
  setupDest' tl id rest
setupDest' tl id (t :>: handler) = do
  addToTargetList tl id t
  return (fromIntegral id, handler)

