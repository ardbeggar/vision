-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 26 Jun. 2010
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

module Clipboard
  ( initClipboard
  , clipboard
  , onClipboardTargets
  , getClipboardTargets
  ) where

import Control.Concurrent.MVar
import Control.Monad

import Data.Maybe

import Graphics.UI.Gtk hiding (Clipboard)
import qualified Graphics.UI.Gtk as G

import Env
import Handler
import Utils


data State
  = State { sTargets :: [TargetTag] }

makeState =
  State { sTargets = [] }

data Clipboard
  = Clipboard { cState              :: MVar State
              , cClipboard          :: G.Clipboard
              , cOnClipboardTargets :: HandlerMVar ()
              }

state = cState getEnv

clipboard = cClipboard getEnv

onClipboardTargets = onHandler $ cOnClipboardTargets getEnv

getClipboardTargets = withMVar state $ return . sTargets

updateClipboardTargets targets = do
  let targets' = fromMaybe [] targets
  modifyMVar state $ \state ->
    return (state { sTargets = targets' }, sTargets state /= targets')


initClipboard = do
  env <- initEnv
  let ?env = env

  timeoutAdd checkClipboard 0

  return ?env


initEnv = do
  state              <- newMVar makeState
  clipboard          <- clipboardGet selectionClipboard
  onClipboardTargets <- makeHandlerMVar
  return $ augmentEnv
    Clipboard { cState              = state
              , cClipboard          = clipboard
              , cOnClipboardTargets = onClipboardTargets
              }

checkClipboard = do
  clipboard <- clipboardGet selectionClipboard
  clipboardRequestTargets clipboard $ \targets -> do
    changed <- updateClipboardTargets targets
    when changed $ onClipboardTargets $ invoke ()
    timeoutAdd checkClipboard 250
    return ()
  return False
