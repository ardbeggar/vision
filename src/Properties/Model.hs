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
  , propertyList
  , propertyMap
  , getProperties
  , setProperties
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as Map

import Context
import Config
import Properties.Property


data Model
  = Model { mMap   :: MVar (Map String Property) }

propertyMap = mMap context


initModel = do
  context <- initContext
  let ?context = context

  loadProperties

  return ?context

initContext = do
  npmap <- newMVar $ Map.fromList $ map (\p -> (propName p, p)) builtinProperties
  return $ augmentContext
    Model { mMap = npmap }

property name =
  withMVar propertyMap $ return . Map.lookup name

propertyList =
  withMVar propertyMap $ return . Map.elems

loadProperties = do
  props <- config "properties.conf" []
  modifyMVar_ propertyMap $ \m ->
    return . Map.union m . Map.fromList $ map (\p -> (propName p, p)) props

getProperties =
  withMVar propertyMap $ return . Map.elems

setProperties props = do
  modifyMVar_ propertyMap $ const $ return $
    Map.fromList $ map (\p -> (propName p, p)) props
  writeConfig "properties.conf" props
  return ()

