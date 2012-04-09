-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}

module XMMS
  ( initXMMS
  , WithXMMS
  , withXMMS
  , xmms
  , connected
  , connectedV
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Typeable
import Data.Env

import Graphics.UI.Gtk

import XMMS2.Client
import XMMS2.Client.Glib

import Environment
import Registry


data Ix = Ix deriving (Typeable)

data XMMS
  = XMMS { _xmms      :: Connection
         , _connected :: TGVar Bool
         }
    deriving (Typeable)

type WithXMMS = ?_XMMS :: XMMS

xmms :: WithXMMS => Connection
xmms = _xmms ?_XMMS

connectedV :: WithXMMS => TGVar Bool
connectedV = _connected ?_XMMS

connected :: WithXMMS => IO Bool
connected  = readTGVarIO connectedV

initXMMS :: (WithRegistry, WithEnvironment) => IO ()
initXMMS = do
  xmms <- mkXMMS
  addEnv Ix xmms
  let ?_XMMS = xmms
  scheduleTryConnect 100
  return ()

withXMMS :: WithRegistry => (WithXMMS => IO a) -> IO a
withXMMS func = do
  Just (Env xmms) <- getEnv (Extract :: Extract Ix XMMS)
  let ?_XMMS = xmms
  func

mkXMMS :: IO XMMS
mkXMMS = do
  xmms      <- XMMS2.Client.init "Vision"
  connected <- atomically $ newTGVar False
  return XMMS { _xmms      = xmms
              , _connected = connected
              }

scheduleTryConnect :: (WithEnvironment, WithXMMS) => Int -> IO HandlerId
scheduleTryConnect =
  timeoutAdd tryConnect

tryConnect :: (WithEnvironment, WithXMMS) => IO Bool
tryConnect = do
  success <- connect xmms xmmsPath
  if success
    then do
    disconnectCallbackSet xmms disconnectCallback
    mainLoopGMainInit xmms
    setConnected True
    yield
    return False
    else do
    scheduleTryConnect 1000
    return False

disconnectCallback :: (WithEnvironment, WithXMMS) => IO ()
disconnectCallback = do
  setConnected False
  yield
  scheduleTryConnect 1000
  return ()

setConnected :: WithXMMS => Bool -> IO ()
setConnected =
  atomically . writeTGVar connectedV
