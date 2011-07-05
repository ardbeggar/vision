-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection
  ( initCollection
  , browseCollection
  ) where

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import UI

import Collection.List
import Collection.ScrollBox


initCollection =
  initList

browseCollection _maybeName = do
  builder <- liftIO $ makeBuilder "collection-browser"
  context <- liftIO $ initUI builder
  let ?context = context

  withListView $ do
    view <- listView
    liftIO $ do
      box    <- builderGetObject builder castToVBox "views"
      scroll <- scrolledWindowNew Nothing Nothing
      scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
      boxPackStartDefaults box scroll
      sbox <- mkScrollBox
      containerAdd scroll $ sViewport sbox
      scroll <- scrolledWindowNew Nothing Nothing
      scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
      scrollBoxAdd sbox scroll
      containerAdd scroll view
      postGUIAsync $ forM_ [0 .. 10] $ const $ do
        b <- buttonNewWithLabel $ take 50 ['a', 'a' .. ]
        widgetShowAll b
        scrollBoxAdd sbox b
    return ()

  liftIO $ widgetShowAll window
