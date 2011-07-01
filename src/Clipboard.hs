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
  , clipboardTargets
  , getClipboardTargets
  ) where

import Control.Concurrent.STM

import Data.Maybe

import Graphics.UI.Gtk hiding (Clipboard)
import qualified Graphics.UI.Gtk as G

import Context


data Clipboard
  = Clipboard { cTargets            :: TVar [TargetTag]
              , cClipboard          :: G.Clipboard
              }

clipboard        = cClipboard context
clipboardTargets = cTargets context

getClipboardTargets = readTVarIO clipboardTargets

updateClipboardTargets =
  atomically . writeTVar clipboardTargets . fromMaybe []


initClipboard = do
  context <- initContext
  let ?context = context

  timeoutAdd checkClipboard 0

  return ?context


initContext = do
  targets            <- newTVarIO []
  clipboard          <- clipboardGet selectionClipboard
  return $ augmentContext
    Clipboard { cTargets            = targets
              , cClipboard          = clipboard
              }

checkClipboard = do
  clipboard <- clipboardGet selectionClipboard
  clipboardRequestTargets clipboard $ \targets -> do
    updateClipboardTargets targets
    timeoutAdd checkClipboard 250
    return ()
  return False
