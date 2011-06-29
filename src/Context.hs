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
             OverlappingInstances,
             TypeOperators,
             DeriveDataTypeable,
             UndecidableInstances #-}

module Context
  ( ContextClass
  , ContextType
  , makeContext
  , augmentContext
  , context
  , EnvM
  , runEnvT
  , runIn
  , (&>)
  , ($>)
  , startRegistry
  , registryEnv
  , RegistryEnvOp
  , addEnv
  , getEnv
  ) where

import Control.Monad.ReaderX
import Control.Monad.ToIO
import Control.Monad.W
import Prelude hiding (catch)
import Control.Monad.CatchIO

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM

import Data.Typeable
import Data.Dynamic


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


class    MonadReaderX ix r m => EnvM ix r m
instance MonadReaderX ix r m => EnvM ix r m

runEnvT :: Index ix => (ix, r) -> ReaderTX ix r m a -> m a
runEnvT (ix, r) f = runReaderTX ix f r

instance (Index ix, MonadCatchIO m) =>
         MonadCatchIO (ReaderTX ix env m) where
  m `catch` f = mkReaderTX getVal $ \r ->
    (runReaderTX getVal m r)
    `catch`
    (\e -> runReaderTX getVal (f e) r)
  block       = mapReaderTX getVal block
  unblock     = mapReaderTX getVal unblock

instance (Index ix, ToIO m) => ToIO (ReaderTX ix r m) where
  toIO = do
    let ix = getVal
    r <- askx ix
    t <- lift toIO
    return $ W $ \m ->
      runW t $ runReaderTX ix m r

runIn ::
  MonadReaderX ix r m
  => (ix, r)
  -> m (W (ReaderTX ix r n) n)
runIn _ = do
  let ix = getVal
  b <- askx ix
  return $ W $ \m ->
    runReaderTX ix m b

(&>) ::
  MonadReaderX ix r m =>
  m (W n n1) -> (ix, r) -> m (W (ReaderTX ix r n) n1)
a &> b = do
  a' <- a
  b' <- runIn b
  return $ W $ runW a' . runW b'

($>) :: MonadIO m => m (W n IO) -> n b -> m b
r $> f = do
  r' <- r
  liftIO $ runW r' f

data Ix = Ix deriving Typeable
instance Index Ix where getVal = Ix

type EnvMap = TVar (IntMap Dynamic)

registryEnv :: (Ix, EnvMap)
registryEnv = undefined

class    (EnvM Ix EnvMap m, MonadIO m) => RegistryM m
instance (EnvM Ix EnvMap m, MonadIO m) => RegistryM m

startRegistry :: ReaderTX Ix EnvMap IO a -> IO a
startRegistry f = do
  v <- newTVarIO $ IntMap.empty
  runEnvT (Ix, v) (addEnv Ix v >> getEnv (Ix, v) >> f)

class    (RegistryM m, Index ix, Typeable ix, Typeable r) => RegistryEnvOp ix r m
instance (RegistryM m, Index ix, Typeable ix, Typeable r) => RegistryEnvOp ix r m

addEnv :: RegistryEnvOp ix r m => ix -> r -> m ()
addEnv ix r = do
  let val = (ix, r)
  var <- askx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf val
    atomically $ do
      map <- readTVar var
      writeTVar var $ IntMap.insert key (toDyn val) map

getEnv :: RegistryEnvOp ix r m => (ix, r) -> m (Maybe (ix, r))
getEnv spec = do
  var <- askx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf spec
    atomically $ do
      map <- readTVar var
      case IntMap.lookup key map of
        Nothing -> return Nothing
        Just dv -> return $ fromDynamic dv
