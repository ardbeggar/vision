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
  , onConnected
  , onDisconnected
  ) where

import Prelude hiding (init)

import Graphics.UI.Gtk

import XMMS2.Client
import XMMS2.Client.Glib

import Environment
import Handler
import Utils
import Env


data XMMS
  = XMMS { xXMMS           :: Connection
         , xOnConnected    :: HandlerMVar ()
         , xOnDisconnected :: HandlerMVar () }

xmms           = xXMMS getEnv
onConnected    = onHandler (xOnConnected getEnv)
onDisconnected = onHandler (xOnDisconnected getEnv)

initXMMS = do
  env <- initEnv
  let ?env = env

  timeoutAdd (onDisconnected (invoke ()) >> return False) 0
  scheduleTryConnect 100

  return ?env


initEnv = do
  xmms           <- init "Vision"
  onConnected    <- makeHandlerMVar
  onDisconnected <- makeHandlerMVar
  return $ augmentEnv
    XMMS { xXMMS           = xmms
         , xOnConnected    = onConnected
         , xOnDisconnected = onDisconnected }

scheduleTryConnect = timeoutAdd tryConnect

tryConnect = do
  putStr "connecting... "
  success <- connect xmms xmmsPath
  if success
    then do
    putStrLn "connected!"
    disconnectCallbackSet xmms disconnectCallback
    mainLoopGMainInit xmms
    onConnected $ invoke ()
    return False
    else do
    putStrLn "failed."
    scheduleTryConnect 1000
    return False

disconnectCallback = do
  putStrLn "disconnected."
  onDisconnected $ invoke ()
  scheduleTryConnect 1000
  return ()
