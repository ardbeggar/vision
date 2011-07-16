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

{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Clipboard
  ( initClipboard
  , withClipboard
  , clipboard
  , clipboardTargets
  , getClipboardTargets
  , copyIds
  ) where

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.EnvIO

import Data.Maybe
import Data.Typeable

import Graphics.UI.Gtk

import Registry
import Atoms (xmms2MlibIdTarget)

data Ix = Ix deriving (Typeable)

data C = C { _targets   :: TVar [TargetTag]
           , _clipboard :: Clipboard
           }
       deriving (Typeable)

clipboardEnv :: Extract Ix C
clipboardEnv = Extract

clipboard        = _clipboard ?clipboard
clipboardTargets = _targets ?clipboard

getClipboardTargets =
  liftIO $ readTVarIO clipboardTargets

copyIds ids = liftIO $ do
  clipboardSetWithData clipboard
    [(xmms2MlibIdTarget, 0)]
    (const $ selectionDataSet selectionTypeInteger ids)
    (return ())
  return ()

updateClipboardTargets ts =
  liftIO $ atomically $ writeTVar clipboardTargets $ fromMaybe [] ts

initClipboard = do
  c <- makeC
  addEnv Ix c
  let ?clipboard = c
  liftIO $ timeoutAdd checkClipboard 0
  return ()

newtype Wrap a = Wrap { unWrap :: (?clipboard :: C) => a }

withClipboard    = withClipboard' . Wrap
withClipboard' w = do
  Just (Env c) <- getEnv clipboardEnv
  let ?clipboard = c in unWrap w

makeC = liftIO $ do
  targets   <- newTVarIO []
  clipboard <- clipboardGet selectionClipboard
  return C { _targets   = targets
           , _clipboard = clipboard
           }

checkClipboard = liftIO $ do
  clipboardRequestTargets clipboard $ \targets -> do
    updateClipboardTargets targets
    liftIO $ timeoutAdd checkClipboard 250
    return ()
  return False
