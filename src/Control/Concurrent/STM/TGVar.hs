-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 30 Jun. 2011
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

module Control.Concurrent.STM.TGVar
  ( TGVar
  , newTGVar
  , readTGVar
  , readTGVarIO
  , writeTGVar
  , TGWatch
  , newTGWatch
  , watchG
  ) where

import Control.Concurrent.STM
import Control.Applicative


data TGVar a = T (TVar (a, Integer))

newTGVar x = T <$> newTVar (x, 1)

readTGVar (T v) = fst <$> readTVar v

readTGVarIO (T v) = fst <$> readTVarIO v

writeTGVar (T v) x = do
  (_, g) <- readTVar v
  writeTVar v (x, g + 1)


data TGWatch a = W (TGVar a) (TVar Integer)

newTGWatch v = W v <$> newTVar 0

watchG (W (T v) g) = do
  this      <- readTVar g
  (x, that) <- readTVar v
  if this /= that
    then do
    writeTVar g that
    return x
    else retry
