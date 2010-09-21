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
  ( getTargetRow
  , reorder
  , selectionDataGetStringList
  , selectionDataSetStringList
  ) where

import Control.Applicative

import Data.List
import Data.Char

import Graphics.UI.Gtk


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

