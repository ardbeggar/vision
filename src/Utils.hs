-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Jun. 2010
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

module Utils
  ( MVar
  , newMVar
  , takeMVar
  , putMVar
  , modifyMVar
  ) where

import Control.Monad.CatchIO
import Control.Monad.Trans
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar


newMVar :: MonadIO m => a -> m (MVar a)
newMVar = liftIO . MVar.newMVar

takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . MVar.takeMVar

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar v a = liftIO $ MVar.putMVar v a

{-# INLINE modifyMVar #-}
modifyMVar :: MonadCatchIO m => MVar a -> (a -> m (a, b)) -> m b
modifyMVar v m =
  block $ do
    a       <- takeMVar v
    (a', b) <- unblock (m a) `onException` putMVar v a
    putMVar v a'
    return b
