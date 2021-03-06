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

{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}

module Clipboard
  ( initClipboard
  , WithClipboard
  , withClipboard
  , clipboard
  , clipboardTargets
  , getClipboardTargets
  , copyIds
  ) where

import Control.Concurrent.STM
import Control.Monad.Trans

import Data.Env
import Data.Maybe
import Data.Typeable

import Graphics.UI.Gtk

import XMMS2.Client (MediaId)

import Registry
import Atoms (xmms2MlibIdTarget)

data Ix = Ix deriving (Typeable)

data C = C { _targets   :: TVar [TargetTag]
           , _clipboard :: Clipboard
           }
       deriving (Typeable)

clipboardEnv :: Extract Ix C
clipboardEnv = Extract

clipboard :: WithClipboard => Clipboard
clipboard = _clipboard ?_Clipboard

clipboardTargets :: WithClipboard => TVar [TargetTag]
clipboardTargets = _targets ?_Clipboard

getClipboardTargets :: WithClipboard => IO [TargetTag]
getClipboardTargets =
  readTVarIO clipboardTargets

copyIds :: WithClipboard => [MediaId] -> IO ()
copyIds ids = do
  clipboardSetWithData clipboard
    [(xmms2MlibIdTarget, 0)]
    (const $ selectionDataSet selectionTypeInteger ids)
    (return ())
  return ()

updateClipboardTargets :: WithClipboard => Maybe [TargetTag] -> IO ()
updateClipboardTargets ts =
  atomically $ writeTVar clipboardTargets $ fromMaybe [] ts

initClipboard :: WithRegistry => IO ()
initClipboard = do
  c <- makeC
  addEnv Ix c
  let ?_Clipboard = c
  timeoutAdd checkClipboard 0
  return ()

type WithClipboard = ?_Clipboard :: C

withClipboard :: WithRegistry => (WithClipboard => IO a) -> IO a
withClipboard func = do
  Just (Env c) <- getEnv clipboardEnv
  let ?_Clipboard = c
  func

makeC :: IO C
makeC = do
  targets   <- newTVarIO []
  clipboard <- clipboardGet selectionClipboard
  return C { _targets   = targets
           , _clipboard = clipboard
           }

checkClipboard :: WithClipboard => IO Bool
checkClipboard = do
  clipboardRequestTargets clipboard $ \targets -> do
    updateClipboardTargets targets
    liftIO $ timeoutAdd checkClipboard 250
    return ()
  return False
