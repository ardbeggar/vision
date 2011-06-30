-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 3 Jun. 2011
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

module Control.Concurrent.STM.TWatch
  ( TWatch
  , newTWatch
  , newEmptyTWatch
  , watch
  ) where

import Control.Concurrent.STM
import Control.Monad


data Eq a => TWatch a =
  TWatch (TVar a) (TVar (Maybe a))

newTWatch :: Eq a => TVar a -> a -> STM (TWatch a)
newTWatch src = newTWatch' src . Just

newEmptyTWatch :: Eq a => TVar a -> STM (TWatch a)
newEmptyTWatch src = newTWatch' src Nothing

newTWatch' :: Eq a => TVar a -> Maybe a -> STM (TWatch a)
newTWatch' src = liftM (TWatch src) . newTVar

watch :: Eq a => TWatch a -> STM a
watch (TWatch src cur) = do
  srcv <- readTVar src
  curv <- readTVar cur
  case curv of
    Just v | v == srcv -> retry
    _                  -> do
      writeTVar cur $ Just srcv
      return srcv
