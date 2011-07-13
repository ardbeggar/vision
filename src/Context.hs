-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
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

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             OverlappingInstances,
             TypeOperators,
             DeriveDataTypeable,
             UndecidableInstances,
             ImplicitParams,
             StandaloneDeriving #-}

module Context
  ( ContextClass
  , ContextType
  , makeContext
  , augmentContext
  , context
  , EnvM
  , runIn
  , startRegistry
  , registryEnv
  , RegistryEnvOp
  , addEnv
  , getEnv
  ) where

import Control.Monad.Trans
import Control.Monad.EnvIO

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM

import Data.Typeable
import Data.Dynamic
import Data.Env


infixr 9 :*
data a :* b = a :* b
data Nil = Nil

class ContextClass c a where
  ctxt :: a -> c

instance ContextClass c (c :* b) where
  ctxt (c :* _) = c

instance ContextClass c b => ContextClass c (a :* b) where
  ctxt (_ :* b) = ctxt b


type ContextType a b = a :* b


makeContext :: c -> ContextType c Nil
makeContext c = c :* Nil

augmentContext :: (?context :: c1) => c2 -> ContextType c2 c1
augmentContext c = c :* ?context

context :: (?context :: a, ContextClass c a) => c
context = ctxt ?context


deriving instance Typeable2 Env

data Ix = Ix deriving Typeable

type EnvMap = TVar (IntMap Dynamic)

registryEnv :: Extract Ix EnvMap
registryEnv = Extract

class    (EnvM Ix EnvMap m) => RegistryM m
instance (EnvM Ix EnvMap m) => RegistryM m

startRegistry f = do
  v <- newTVarIO $ IntMap.empty
  runEnvIO (addEnv Ix v >> f) $ build () $ mkEnv Ix v

class    (RegistryM m, Typeable ix, Typeable a) => RegistryEnvOp ix a m
instance (RegistryM m, Typeable ix, Typeable a) => RegistryEnvOp ix a m

addEnv :: RegistryEnvOp ix a m => ix -> a -> m ()
addEnv ix r = do
  let val = mkEnv ix r
  var <- envx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf val
    atomically $ do
      map <- readTVar var
      writeTVar var $ IntMap.insert key (toDyn val) map

getEnv :: RegistryEnvOp ix a m => Extract ix a -> m (Maybe (Env ix a))
getEnv spec = do
  var <- envx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf $ spec' spec
    atomically $ do
      map <- readTVar var
      case IntMap.lookup key map of
        Nothing -> return Nothing
        Just dv -> return $ fromDynamic dv
  where spec' :: Extract ix a -> Env ix a
        spec' = const undefined