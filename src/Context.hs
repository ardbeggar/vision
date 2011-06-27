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
  , runIn
  , (<&>)
  , startEnvM
  , addEnv
  , getEnv
  , envEnv
  ) where

import Control.Monad.ReaderX
import Control.Monad.ToIO
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


instance (Index ix, MonadCatchIO m) =>
         MonadCatchIO (ReaderTX ix env m) where
  m `catch` f = mkReaderTX getVal $ \r ->
    (runReaderTX getVal m r)
    `catch`
    (\e -> runReaderTX getVal (f e) r)
  block       = mapReaderTX getVal block
  unblock     = mapReaderTX getVal unblock

instance (Index ix, ToIO m) => ToIO (ReaderTX ix env m) where
  toIO = do
    let val = getVal
    env <- askx val
    t <- lift toIO
    return $ \rx -> t $ runReaderTX val rx env

runIn ::
  MonadReaderX ix r m
  => (ix, r)
  -> m (ReaderTX ix r n a -> n a)
runIn _ = do
  let a = getVal
  b <- askx a
  return $ \m ->
    runReaderTX a m b

(<&>) ::
  MonadReaderX ix r m
  => m (n a -> c)
  -> (ix, r)
  -> m (ReaderTX ix r n a -> c)
a <&> b = do
  a' <- a
  b' <- runIn b
  return $ a' . b'


data Ix = Ix deriving Typeable
instance Index Ix where getVal = Ix

type EnvMap = TVar (IntMap Dynamic)

envEnv :: (Ix, EnvMap)
envEnv = undefined

class    (MonadReaderX Ix EnvMap m, MonadIO m) => EnvM m
instance (MonadReaderX Ix EnvMap m, MonadIO m) => EnvM m

startEnvM :: ReaderTX Ix (TVar (IntMap Dynamic)) IO a -> IO a
startEnvM f = do
  v <- newTVarIO $ IntMap.empty
  runReaderTX Ix f v

addEnv :: (EnvM m, Typeable ix, Typeable r) => ix -> r -> m ()
addEnv ix r = do
  let val = (ix, r)
  var <- askx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf val
    atomically $ do
      map <- readTVar var
      writeTVar var $ IntMap.insert key (toDyn val) map

getEnv :: (EnvM m, Typeable ix, Typeable r) => (ix, r) -> m (Maybe (ix, r))
getEnv spec = do
  var <- askx Ix
  liftIO $ do
    key <- typeRepKey $ typeOf spec
    atomically $ do
      map <- readTVar var
      case IntMap.lookup key map of
        Nothing -> return Nothing
        Just dv -> return $ fromDynamic dv
