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

{-# LANGUAGE UndecidableInstances #-}

module XMMS
  ( xmms
  , initXMMS
  , connected
  , connectedV
  , XMMSCC
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Graphics.UI.Gtk

import XMMS2.Client
import XMMS2.Client.Glib

import Environment
import Context


data XMMS
  = XMMS { xXMMS      :: Connection
         , xConnected :: TGVar Bool
         }

class    ContextClass XMMS c => XMMSCC c
instance ContextClass XMMS c => XMMSCC c

xmms       = xXMMS context
connectedV = xConnected context
connected  = readTGVarIO connectedV

initXMMS = do
  context <- initContext
  let ?context = context

  scheduleTryConnect 100

  return ?context


initContext = do
  xmms      <- XMMS2.Client.init "Vision"
  connected <- atomically $ newTGVar False
  return $ augmentContext
    XMMS { xXMMS      = xmms
         , xConnected = connected
         }

scheduleTryConnect = timeoutAdd tryConnect

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

disconnectCallback = do
  setConnected False
  yield
  scheduleTryConnect 1000
  return ()

setConnected = atomically . writeTGVar connectedV
