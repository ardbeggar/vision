-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 15 Sep. 2010
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

module Collection.DnD
  ( setupDnD
  ) where

import Control.Monad.Trans

import Graphics.UI.Gtk

import Atoms

import Collection.Control
import Collection.View


setupDnD = do
  targetList <- targetListNew
  targetListAdd targetList xmms2MlibIdTarget [TargetSameApp] 0

  dragSourceSet collView [Button1] [ActionCopy]
  dragSourceSetTargetList collView targetList

  collView `on` dragDataGet $ \_ _ _ -> do
    ids <- liftIO getSelectedIds
    selectionDataSet selectionTypeInteger ids
    return ()

  return ()

