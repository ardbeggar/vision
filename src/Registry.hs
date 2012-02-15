-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 13 Jul. 2011
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

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             OverlappingInstances,
             TypeOperators,
             DeriveDataTypeable,
             UndecidableInstances,
             ImplicitParams,
             StandaloneDeriving,
             Rank2Types #-}

module Registry
  ( withRegistry
  , addEnv
  , getEnv
  ) where

import Control.Concurrent.STM

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.Dynamic
import Data.Env


deriving instance Typeable2 Env

type EnvMap = TVar (Map TypeRep Dynamic)

newtype Wrap a = Wrap { unWrap :: (?registry :: EnvMap) => a }

withRegistry    = withRegistry' . Wrap
withRegistry' w = do
  registry <- newTVarIO Map.empty
  let ?registry = registry in unWrap w

addEnv :: (?registry :: EnvMap, Typeable ix, Typeable r) => ix -> r -> IO ()
addEnv ix r = do
  let val = mkEnv ix r
  atomically $ do
    map <- readTVar ?registry
    writeTVar ?registry $ Map.insert (typeOf val) (toDyn val) map

getEnv :: (?registry :: EnvMap, Typeable ix, Typeable r) => Extract ix r -> IO (Maybe (Env ix r))
getEnv spec = atomically $ do
  map <- readTVar ?registry
  case Map.lookup (typeOf $ env spec) map of
    Nothing -> return Nothing
    Just dv -> return $ fromDynamic dv
  where env :: Extract ix a -> Env ix a
        env = const undefined
