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
  , addProperty
  , delProperty
  , onPropertyAdded
  , onPropertyDeleted
  , selectIds
  , getSelectedIds
  , onIdsSelected
  ) where

import Prelude hiding (lookup)

import Control.Monad

import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Config
import Callbacks

import Properties.Property


data Properties
  = Properties { propsStore     :: ListStore Property
               , propsMap       :: IORef (Map String Property)
               , propsOnDeleted :: Callbacks (Property -> IO ())
               , propsOnAdded   :: Callbacks (Property -> IO ())
               , pSelectedIds   :: IORef [Int32]
               , pOnIdsSelected :: Callbacks ([Int32] -> IO ()) }

getSelectedIds = readIORef $ pSelectedIds ?properties

selectIds ids = do
  writeIORef (pSelectedIds ?properties) ids
  invokeCallbacks (pOnIdsSelected ?properties) ids

onIdsSelected = addCallback (pOnIdsSelected ?properties)


initModel = do
  store <- listStoreNew []
  npmap <- newIORef $ Map.fromList $
           map (\p -> (propName p, p)) builtinProperties
  onadd <- mkCallbacks
  ondel <- mkCallbacks

  selectedIds   <- newIORef []
  onIdsSelected <- mkCallbacks

  let ?properties = Properties { propsStore     = store
                               , propsMap       = npmap
                               , propsOnAdded   = onadd
                               , propsOnDeleted = ondel
                               , pSelectedIds   = selectedIds
                               , pOnIdsSelected = onIdsSelected }
  loadProperties
  updateProperties
  return ?properties

propertyStore     = propsStore ?properties
propertyMap       = propsMap   ?properties

onPropertyAdded = addCallback (propsOnAdded ?properties)
onPropertyDeleted = addCallback (propsOnDeleted ?properties)

property name = do
  map <- readIORef propertyMap
  return $ Map.lookup name map

propertyList = liftM Map.elems $ readIORef propertyMap

updateProperties = do
  pnmap <- readIORef propertyMap
  listStoreClear propertyStore
  mapM_ (\(_, p) -> listStoreAppend propertyStore p) $ Map.toList pnmap

addProperty prop = do
  modifyIORef propertyMap $ Map.insert (propName prop) prop
  updateProperties
  saveProperties
  invokeCallbacks (propsOnAdded ?properties) prop

delProperty prop = do
  modifyIORef propertyMap $ Map.delete (propName prop)
  updateProperties
  saveProperties
  invokeCallbacks (propsOnDeleted ?properties) prop


saveProperties = do
  props <- listStoreToList propertyStore
  writeConfig "properties.conf" $ filter propCustom props
  return ()

loadProperties = do
  props <- config "properties.conf" []
  npmap <- readIORef propertyMap
  writeIORef propertyMap $ Map.union npmap $ Map.fromList $ map (\p -> (propName p, p)) props
