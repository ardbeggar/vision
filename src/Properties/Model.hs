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

{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}

module Properties.Model
  ( lookup
  , initModel
  , WithModel
  , withModel
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
import Data.Typeable
import Data.Env

import Graphics.UI.Gtk

import Config
import Registry
import Environment
import Properties.Property


data Ix = Ix deriving (Typeable)

data Model
  = Model { _map        :: MVar (Map String Property)
          , _store      :: ListStore Property
          , _generation :: TVar Integer
          }
    deriving (Typeable)

type WithModel = ?_Properties_Model :: Model

propertyMap :: WithModel => MVar (Map String Property)
propertyMap = _map ?_Properties_Model

propertyStore :: WithModel => ListStore Property
propertyStore = _store ?_Properties_Model

propertiesGeneration :: WithModel => TVar Integer
propertiesGeneration = _generation ?_Properties_Model

withModel :: WithRegistry => (WithModel => IO a) -> IO a
withModel func = do
  Just (Env model) <- getEnv (Extract :: Extract Ix Model)
  let ?_Properties_Model = model
  func

initModel :: (WithRegistry, WithEnvironment) => IO ()
initModel = do
  model <- mkModel
  let ?_Properties_Model = model
  loadProperties
  addEnv Ix model

mkModel :: IO Model
mkModel = do
  map        <- newMVar $ mkMap builtinProperties
  store      <- listStoreNewDND [] Nothing Nothing
  generation <- newTVarIO 0
  return Model { _map        = map
               , _store      = store
               , _generation = generation
               }

property :: WithModel => String -> IO (Maybe Property)
property name = withMVar propertyMap $ return . Map.lookup name

loadProperties :: (WithEnvironment, WithModel) => IO ()
loadProperties = do
  props  <- config "properties.conf" []
  mapM_ (listStoreAppend propertyStore) =<<
    modifyMVar propertyMap
    (\m ->
      let m' = Map.union m $ mkMap props in
      return (m', Map.elems m'))

getProperties :: WithModel => IO [Property]
getProperties =
  withMVar propertyMap $ return . Map.elems

setProperties :: (WithEnvironment, WithModel) => [Property] -> IO ()
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

mkMap :: [Property] -> Map String Property
mkMap = Map.fromList . map (\p -> (propName p, p))
