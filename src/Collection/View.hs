-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection.View
  ( initView
  , collView
  ) where

import Prelude hiding (lookup)

import Graphics.UI.Gtk

import Context
import Properties
import Utils
import Collection.Model


data View
  = View { vView :: TreeView }

collView = vView context


initView = do
  context <- initContext
  let ?context = context

  treeViewSetRulesHint collView True
  addColumns

  return ?context


initContext = do
  view <- treeViewNewWithModel collStore
  return $ augmentContext
    View { vView = view }

addColumns =
  mapM_ addOne ["Artist", "Album", "Track", "Title"]
  where addOne name = do
          prop <- property name
          fmaybeM_ prop addColumn

addColumn prop = do
  column <- treeViewColumnNew
  treeViewAppendColumn collView column
  treeViewColumnSetTitle column $ propName prop
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell collStore $ \iter -> do
    maybeInfo <- getInfoIfNeeded iter
    let text = case maybeInfo of
          Just info -> maybe "" id (lookup prop info)
          Nothing   -> ""
    cell `set` [ cellText := text ]

getInfoIfNeeded iter = do
  [n] <- treeModelGetPath collStore iter
  mid <- listStoreGetValue collStore n
  rng <- treeViewGetVisibleRange collView
  getInfo mid $ case rng of
    ([f], [t]) -> n >= f && t >= n
    _          -> False
