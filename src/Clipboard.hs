-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 26 Jun. 2010
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

{-# LANGUAGE DeriveDataTypeable, NoMonoLocalBinds #-}

module Clipboard
  ( initClipboard
  , clipboard
  , onClipboardTargets
  , getClipboardTargets
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Index
import Control.Monad.ReaderX
import Control.Monad.ToIO

import Data.Maybe
import Data.Typeable

import Graphics.UI.Gtk

import Context
import Handler
import Utils


data State
  = State { sTargets :: [TargetTag] }

makeState =
  State { sTargets = [] }

data Ix = Ix deriving (Typeable)
instance Index Ix where getVal = Ix

data Env
  = Env { cState              :: MVar State
        , cClipboard          :: Clipboard
        , cOnClipboardTargets :: HandlerMVar ()
        }
    deriving (Typeable)

clipboardEnv :: (Ix, Env)
clipboardEnv = undefined

clipboard = asksx Ix cClipboard

onClipboardTargets f = do
  handler <- asksx Ix cOnClipboardTargets
  liftIO $ onHandler handler f

getClipboardTargets = asksx Ix cState >>= \state ->
  liftIO $ withMVar state $ return . sTargets

updateClipboardTargets targets = do
  state <- asksx Ix cState
  let targets' = fromMaybe [] targets
  liftIO $ modifyMVar state $ \state ->
    return (state { sTargets = targets' }, sTargets state /= targets')


initClipboard = do
  env <- makeEnv
  addEnv Ix env
  runEnvT (Ix, env) $ runIn clipboardEnv >>= \run ->
    liftIO $ timeoutAdd (run checkClipboard) 0
  return ()

makeEnv = liftIO $ do
  state              <- newMVar makeState
  clipboard          <- clipboardGet selectionClipboard
  onClipboardTargets <- makeHandlerMVar
  return Env { cState              = state
             , cClipboard          = clipboard
             , cOnClipboardTargets = onClipboardTargets
             }

checkClipboard = do
  clipboard <- asksx Ix cClipboard
  io $ \run ->
    clipboardRequestTargets clipboard $ \targets -> run $ do
      changed <- updateClipboardTargets targets
      when changed $ onClipboardTargets $ invoke ()
      io $ \run -> timeoutAdd (run checkClipboard) 250
      return ()
  return False
