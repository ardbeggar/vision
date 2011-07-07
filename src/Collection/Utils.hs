-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

module Collection.Utils
  ( selectAll
  , invertSelection
  , setupViewFocus
  ) where

import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (selectAll)

import Collection.Actions


selectAll =
  treeSelectionSelectAll

invertSelection sel = do
  rows <- treeSelectionGetSelectedRows sel
  treeSelectionSelectAll sel
  mapM_ (treeSelectionUnselectPath sel) rows

setupViewFocus abRef view ab = do
  view `on` focusInEvent $ liftIO $ do
    writeIORef abRef ab
    return False
  view `on` focusOutEvent $ liftIO $ do
    writeIORef abRef emptyAB
    return False
