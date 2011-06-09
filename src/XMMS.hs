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
  , SN ()
  , mkSN
  , activateSN
  , waitSN
  , doneSN
  ) where

import Prelude hiding (init)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Data.Unique

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client
import XMMS2.Client.Glib

import Environment
import Handler
import Utils
import Context


data XMMS
  = XMMS { xXMMS               :: Connection
         , xOnServerConnection :: HandlerMVar Bool
         , xConnected          :: TVar Bool
         , xConnectionId       :: TVar Unique
         , xNWatchers          :: TVar Int
         , xNWaiting           :: TVar Int
         }

xmms               = xXMMS context
onServerConnection = onHandler (xOnServerConnection context)

connected = readTVarIO $ xConnected context

initXMMS = do
  context <- initContext
  let ?context = context

  scheduleTryConnect 100

  return ?context


initContext = do
  xmms               <- init "Vision"
  onServerConnection <- makeHandlerMVar
  connected          <- atomically $ newTVar False
  id                 <- newUnique
  connectionId       <- atomically $ newTVar id
  nWatchers          <- atomically $ newTVar 0
  nWaiting           <- atomically $ newTVar 0
  return $ augmentContext
    XMMS { xXMMS               = xmms
         , xOnServerConnection = onServerConnection
         , xConnected          = connected
         , xConnectionId       = connectionId
         , xNWatchers          = nWatchers
         , xNWaiting           = nWaiting
         }

scheduleTryConnect = timeoutAdd tryConnect

tryConnect = do
  success <- connect xmms xmmsPath
  if success
    then do
    disconnectCallbackSet xmms disconnectCallback
    mainLoopGMainInit xmms
    setConnected True
    onServerConnection $ invoke True
    return False
    else do
    scheduleTryConnect 1000
    return False

disconnectCallback = do
  setConnected False
  onServerConnection $ invoke False
  scheduleTryConnect 1000
  return ()

onServerConnectionAdd f = do
  id <- onServerConnection . add $ f
  f =<< connected
  return id

setConnected conn = do
  putStrLn $ "Notify: " ++ show conn
  id <- newUnique
  atomically $ do
    writeTVar (xConnected context) conn
    writeTVar (xConnectionId context) id
    writeTVar (xNWaiting context) =<< readTVar (xNWatchers context)
  yield
  atomically $ do
    nw <- readTVar $ xNWaiting context
    if nw == 0
      then return ()
      else retry
  putStrLn "done"


data SN a =
  SN { snValue   :: TVar a
     , snValueId :: TVar Unique
     , snOurId   :: TVar (Maybe Unique)
     , snNWatch  :: TVar Int
     , snNWait   :: TVar Int
     }

mkSN = do
  ourId <- newTVar Nothing
  return SN { snValue   = xConnected context
            , snValueId = xConnectionId context
            , snOurId   = ourId
            , snNWatch  = xNWatchers context
            , snNWait   = xNWaiting context
            }

activateSN sn = do
  nw <- readTVar (snNWatch sn)
  writeTVar (snNWatch sn) (nw + 1)

waitSN sn = do
  oid <- readTVar (snOurId sn)
  vid <- Just <$> readTVar (snValueId sn)
  if oid == vid
    then retry
    else do
    writeTVar (snOurId sn) vid
    readTVar (snValue sn)

doneSN sn = do
  nw <- readTVar (snNWait sn)
  writeTVar (snNWait sn) (nw - 1)
