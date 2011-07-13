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
             UndecidableInstances #-}

module Control.Monad.EnvIO
  ( EnvM (..)
  , EnvIO (..)
  , env
  , runIn
  , runIn'
  , ($>)
  ) where

import Control.Applicative

import Control.Monad.Trans
import Control.Monad.W
import Control.Monad.ToIO

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

instance EnvX ix a e => EnvM ix a (EnvIO e) where
  envx ix = extract ix <$> env


env :: EnvIO e e
env = EnvIO $ return

runIn :: EnvB ix a e e' b => b -> EnvIO e (W (EnvIO e') IO)
runIn t =
  runIn' $ flip build t

runIn' :: (e -> e') -> EnvIO e (W (EnvIO e') IO)
runIn' f = do
  e <- env
  return $ W $ \m ->
    runEnvIO m $ f e

-- TODO: move to Control.Monad.W
($>) :: MonadIO m => m (W n IO) -> n b -> m b
w $> f = do
  W run <- w
  liftIO $ run f
infixr 0 $>
