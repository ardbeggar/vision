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
  , onProperties
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.MVar

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import Context
import Config
import Handler
import Utils
import Properties.Property


data Model
  = Model { mMap          :: MVar (Map String Property)
          , mStore        :: ListStore Property
          , mOnProperties :: HandlerMVar ()
          }

propertyMap   = mMap context
propertyStore = mStore context
onProperties  = onHandler $ mOnProperties context


initModel = do
  context <- initContext
  let ?context = context

  loadProperties

  return ?context

initContext = do
  npmap   <- newMVar $ mkMap builtinProperties
  -- The store will be populated in loadProperties.
  store   <- listStoreNewDND [] Nothing Nothing
  onprops <- makeHandlerMVar
  return $ augmentContext
    Model { mMap          = npmap
          , mStore        = store
          , mOnProperties = onprops
          }

property name =
  withMVar propertyMap $ return . Map.lookup name

loadProperties = do
  props  <- config "properties.conf" []
  mapM_ (listStoreAppend propertyStore) =<<
    (modifyMVar propertyMap $ \m -> do
        let m' = Map.union m $ mkMap props
        return (m', Map.elems m'))

getProperties =
  withMVar propertyMap $ return . Map.elems

setProperties props = do
  listStoreClear propertyStore
  mapM_ (listStoreAppend propertyStore) =<<
    (modifyMVar propertyMap $ const $ do
        let m = mkMap props
        return (m, Map.elems m))
  writeConfig "properties.conf" props
  onProperties $ invoke ()


mkMap = Map.fromList . map (\p -> (propName p, p))
