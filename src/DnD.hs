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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module DnD
  ( setupDnDReorder
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk


setupDnDReorder target store view apply = do
  sel <- treeViewGetSelection view

  targetList <- targetListNew
  targetListAdd targetList target [TargetSameWidget] 0

  dropRef <- newIORef False

  dragSourceSet view [Button1] [ActionMove]
  dragSourceSetTargetList view targetList

  view `on` dragDataGet $ \_ _ _ -> do
    rows <- liftIO $ treeSelectionGetSelectedRows sel
    selectionDataSet selectionTypeInteger $ map head rows
    return ()

  dragDestSet view [DestDefaultMotion, DestDefaultHighlight] [ActionMove]
  dragDestSetTargetList view targetList

  view `on` dragDrop $ \ctxt _ tstamp -> do
    writeIORef dropRef True
    dragGetData view ctxt target tstamp
    return True

  view `on` dragDataReceived $ \ctxt (_, y) _ tstamp -> do
    drop <- liftIO $ readIORef dropRef
    when drop $ do
      liftIO $ writeIORef dropRef False
      (rows :: Maybe [Int]) <- selectionDataGet selectionTypeInteger
      liftIO $ doReorder apply store view y rows
      liftIO $ dragFinish ctxt True True tstamp

  view `on` dragDataDelete $ \_ ->
    treeSelectionUnselectAll sel


doReorder _ _ _ _ Nothing                  = return ()
doReorder apply store view y (Just rows) = do
  base <- getTargetRow
  apply $ reorder base rows
  where getTargetRow = do
          maybePos <- treeViewGetPathAtPos view (0, y)
          case maybePos of
            Just ([n], _, _) ->
              return n
            Nothing ->
              pred <$> listStoreGetSize store

reorder = reorderDown 0
  where reorderDown _ _ [] = []
        reorderDown dec base rows@(r:rs)
          | r <= base = (r - dec, base) : reorderDown (dec + 1) base rs
          | otherwise = reorderUp (if dec /= 0 then base + 1 else base) rows
        reorderUp _ [] = []
        reorderUp base (r:rs)
          | r == base = reorderUp (base + 1) rs
          | otherwise = (r, base) : reorderUp (base + 1) rs

