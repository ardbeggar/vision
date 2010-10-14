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

module XMMS
  ( xmms
  , initXMMS
  , connected
  , onServerConnection
  , onServerConnectionAdd
  ) where

import Prelude hiding (init)
import Control.Concurrent.MVar

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client
import XMMS2.Client.Glib

import Environment
import Handler
import Utils
import Context


data State
  = State { sConnected :: Bool }

makeState = State { sConnected = False }


data XMMS
  = XMMS { xXMMS               :: Connection
         , xOnServerConnection :: HandlerMVar Bool
         , xState              :: MVar State
         }

xmms               = xXMMS context
onServerConnection = onHandler (xOnServerConnection context)
state              = xState context

connected = withMVar state $ return . sConnected

initXMMS = do
  context <- initContext
  let ?context = context

  scheduleTryConnect 100

  return ?context


initContext = do
  xmms               <- init "Vision"
  onServerConnection <- makeHandlerMVar
  state              <- newMVar makeState
  return $ augmentContext
    XMMS { xXMMS               = xmms
         , xOnServerConnection = onServerConnection
         , xState              = state
         }

scheduleTryConnect = timeoutAdd tryConnect

tryConnect = do
  success <- connect xmms xmmsPath
  if success
    then do
    disconnectCallbackSet xmms disconnectCallback
    mainLoopGMainInit xmms
    modifyMVar_ state $ \s -> return s { sConnected = True }
    onServerConnection $ invoke True
    return False
    else do
    scheduleTryConnect 1000
    return False

disconnectCallback = do
  modifyMVar_ state $ \s -> return s { sConnected = False }
  onServerConnection $ invoke False
  scheduleTryConnect 1000
  return ()

onServerConnectionAdd f = do
  id <- onServerConnection . add $ f
  f =<< connected
  return id
