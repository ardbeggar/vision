-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 4 Mar. 2009
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

module Properties.Model
  ( lookup
  , initModel
  , propertyStore
  , property
  , propertyList
  , propertyMap
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Env
import Config

import Properties.Property


data Properties
  = Properties { pStore :: ListStore Property
               , pMap   :: MVar (Map String Property)
               }

propertyStore = pStore getEnv
propertyMap   = pMap getEnv


initModel = do
  env <- initEnv
  let ?env = env

  loadProperties
  updateProperties

  return ?env

initEnv = do
  store <- listStoreNew []
  npmap <- newMVar $ Map.fromList $ map (\p -> (propName p, p)) builtinProperties
  return $ augmentEnv
    Properties { pStore = store
               , pMap   = npmap
               }

property name =
  withMVar propertyMap $ return . Map.lookup name

propertyList =
  withMVar propertyMap $ return . Map.elems

updateProperties =
  withMVar propertyMap $ \m -> do
    listStoreClear propertyStore
    mapM_ (\(_, p) -> listStoreAppend propertyStore p) $ Map.toList m

loadProperties = do
  props <- config "properties.conf" []
  modifyMVar_ propertyMap $ \m ->
    return . Map.union m . Map.fromList $ map (\p -> (propName p, p)) props
