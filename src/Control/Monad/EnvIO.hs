-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 13 Jul. 2010
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
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances #-}

module Control.Monad.EnvIO
  ( module Data.Env
  , module Control.Monad.Trans
  , module Control.Monad.ToIO
  , module Control.Monad.W
  , EnvM (..)
  , envsx
  , EnvIO (..)
  , env
  , runIn
  , runIn'
  ) where

import Prelude hiding (catch)

import Control.Applicative

import Control.Monad.Trans
import Control.Monad.W
import Control.Monad.ToIO
import Control.Monad.CatchIO

import Data.Env


class (Monad m, MonadIO m) => EnvM ix a m | ix -> m a where
  envx :: ix -> m a


newtype EnvIO e a = EnvIO { runEnvIO :: e -> IO a }

instance Functor (EnvIO e) where
  f `fmap` m = EnvIO $ \e -> do
    a <- runEnvIO m e
    return $ f a

instance Monad (EnvIO e) where
  return a = EnvIO $ const $ return a
  m >>= n  = EnvIO $ \e -> do
    a <- runEnvIO m e
    runEnvIO (n a) e

instance MonadIO (EnvIO e) where
  liftIO = EnvIO . const

instance ToIO (EnvIO e) where
  toIO = do
    e <- env
    return $ W $ flip runEnvIO e

instance MonadCatchIO (EnvIO e) where
  m `catch` f = EnvIO $ \e ->
    (runEnvIO m e)
    `catch`
    (\x -> runEnvIO (f x) e)
  block       = mapEnvIO block
  unblock     = mapEnvIO unblock

instance EnvX ix a e => EnvM ix a (EnvIO e) where
  envx ix = extract ix <$> env


env :: EnvIO e e
env = EnvIO $ return

mapEnvIO :: (IO a -> IO b) -> EnvIO e a -> EnvIO e b
mapEnvIO f m = EnvIO $ f . runEnvIO m

runIn :: EnvB ix a e e' b => b -> EnvIO e (W (EnvIO e') IO)
runIn t =
  runIn' $ flip build t

runIn' :: (e -> e') -> EnvIO e (W (EnvIO e') IO)
runIn' f = do
  e <- env
  return $ W $ \m ->
    runEnvIO m $ f e

envsx :: EnvX ix a e => ix -> (a -> b) -> EnvIO e b
envsx ix acc = acc <$> envx ix
