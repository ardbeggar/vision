-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 10 Sep. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module DnD
  ( TargetClass (..)
  , CommonTargets (..)
  , Targets (..)
  , DragDest (..)
  , setupDragDest
  , getTargetRow
  , reorderRows
  , reorder
  , selectionDataGetStringList
  , selectionDataSetStringList
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.List
import Data.Char
import Data.IORef
import qualified Data.IntMap as IntMap

import Graphics.UI.Gtk

import Utils


class TargetClass t where
  addToTargetList :: TargetList -> InfoId -> t -> IO ()


instance TargetClass TargetTag where
  addToTargetList tl id tg = targetListAdd tl tg [] id


data CommonTargets
  = URITargets

instance TargetClass CommonTargets where
  addToTargetList tl id tg =
    case tg of
      URITargets -> targetListAddUriTargets tl id


data Targets
  = forall t1 t2. (TargetClass t1, TargetClass t2) => t1 :|: t2

infixr 5 :|:

instance TargetClass Targets where
  addToTargetList tl id (t1 :|: t2) = do
    addToTargetList tl id t1
    addToTargetList tl id t2


data DragDest
  = forall t. TargetClass t => t :>: (DragContext -> Point -> SelectionDataM (Bool, Bool))

infix 4 :>:


setupDragDest widget defs acts dests = do
  tl <- targetListNew
  hm <- IntMap.fromList <$> zipWithM (mk tl) [0 .. ] dests

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
        Just handler -> handler ctxt pos
        Nothing      -> return (False, False)
      liftIO $ dragFinish ctxt ok del tstamp

  return ()

  where mk tl id (ts :>: handler) = do
          addToTargetList tl id ts
          return (fromIntegral id, handler)


getTargetRow store view y reorder = do
  maybePos <- treeViewGetPathAtPos view (0, y)
  case maybePos of
    Just ([n], _, _) | reorder ->
      return n
    Just ([n], column, _) -> do
      Rectangle _ cy _ ch <- treeViewGetCellArea view (Just [n]) column
      return $ if y - cy > 2 * ch `div` 3 then n + 1 else n
    Nothing | reorder ->
      pred <$> listStoreGetSize store
    Nothing ->
      listStoreGetSize store

reorderRows store view f _ (_, y) = do
  rows <- selectionDataGet selectionTypeInteger
  liftIO $ withJust rows $ \rows -> do
    base <- getTargetRow store view y True
    f $ reorder base rows
  return (True, True)

reorder = reorderDown 0
  where reorderDown _ _ [] = []
        reorderDown dec base rows@(r:rs)
          | r <= base = (r - dec, base) : reorderDown (dec + 1) base rs
          | otherwise = reorderUp (if dec /= 0 then base + 1 else base) rows
        reorderUp _ [] = []
        reorderUp base (r:rs)
          | r == base = reorderUp (base + 1) rs
          | otherwise = (r, base) : reorderUp (base + 1) rs


selectionDataSetStringList =
  selectionDataSet selectionTypeInteger . intercalate [0] . map (map ord)

selectionDataGetStringList =
  maybe [] brk <$> selectionDataGet selectionTypeInteger
  where brk text = case break (== 0) text of
          (name, [])       -> [map chr name]
          (name, _ : rest) -> map chr name : brk rest

