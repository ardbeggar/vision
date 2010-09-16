-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 16 Sep. 2010
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

module Location.DnD
  ( setupDnD
  ) where

import Control.Monad.Trans

import Network.URL

import Graphics.UI.Gtk

import Location.Model
import Location.View


setupDnD = do
  targetList <- targetListNew
  targetListAddUriTargets targetList 0

  dragSourceSet locationView [Button1] [ActionCopy]
  dragSourceSetTargetList locationView targetList

  locationView `on` dragDataGet $ \_ _ _ -> do
    paths <- liftIO $ do
      rows  <- treeSelectionGetSelectedRows locationSel
      items <- mapM itemByPath rows
      return $ map (encString False ok_url . iPath) items
    selectionDataSetURIs paths
    return ()

  return ()

