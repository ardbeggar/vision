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
  , property
  , propertyMap
  , propertyStore
  , getProperties
  , setProperties
  , propertiesGeneration
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Context
import Config
import Properties.Property


data Model
  = Model { mMap        :: MVar (Map String Property)
          , mStore      :: ListStore Property
          , mGeneration :: TVar Integer
          }

propertyMap          = mMap context
propertyStore        = mStore context
propertiesGeneration = mGeneration context


initModel = do
  context <- initContext
  let ?context = context

  loadProperties

  return ?context

initContext = do
  npmap      <- newMVar $ mkMap builtinProperties
  store      <- listStoreNewDND [] Nothing Nothing
  generation <- newTVarIO 0
  return $ augmentContext
    Model { mMap        = npmap
          , mStore      = store
          , mGeneration = generation
          }

property name =
  withMVar propertyMap $ return . Map.lookup name

loadProperties = do
  props  <- config "properties.conf" []
  mapM_ (listStoreAppend propertyStore) =<<
    modifyMVar propertyMap
    (\m ->
      let m' = Map.union m $ mkMap props in
      return (m', Map.elems m'))

getProperties =
  withMVar propertyMap $ return . Map.elems

setProperties props = do
  listStoreClear propertyStore
  mapM_ (listStoreAppend propertyStore) =<<
    modifyMVar propertyMap
    (const $
     let m = mkMap props in
     return (m, Map.elems m))
  writeConfig "properties.conf" props
  atomically $ do
    g <- readTVar propertiesGeneration
    writeTVar propertiesGeneration $ g + 1


mkMap = Map.fromList . map (\p -> (propName p, p))
