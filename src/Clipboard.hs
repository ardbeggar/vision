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

{-# LANGUAGE DeriveDataTypeable, UndecidableInstances #-}

module Clipboard
  ( initClipboard
  , clipboardEnv
  , clipboard
  , clipboardTargets
  , getClipboardTargets
  , copyIds
  ) where

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.ToIO
import Control.Monad.EnvIO
import Control.Monad.W

import Data.Maybe
import Data.Typeable
import Data.Env hiding (Env)

import Graphics.UI.Gtk

import Registry
import Atoms (xmms2MlibIdTarget)


data Ix = Ix deriving (Typeable)

data Env
  = Env { cTargets            :: TVar [TargetTag]
        , cClipboard          :: Clipboard
        }
    deriving (Typeable)

clipboardEnv :: Extract Ix Env
clipboardEnv = Extract

clipboard        = envsx Ix cClipboard
clipboardTargets = envsx Ix cTargets

getClipboardTargets =
  clipboardTargets >>= liftIO . readTVarIO

copyIds ids = do
  clipboard <- clipboard
  liftIO $ do
    clipboardSetWithData clipboard
      [(xmms2MlibIdTarget, 0)]
      (const $ selectionDataSet selectionTypeInteger ids)
      (return ())
    return ()

updateClipboardTargets ts = do
  targets <- clipboardTargets
  liftIO $ atomically $ writeTVar targets $ fromMaybe [] ts


initClipboard = do
  env <- makeEnv
  addEnv Ix env
  runIn (mkEnv Ix env) $> do
    io $ \run -> timeoutAdd (run checkClipboard) 0
  return ()

makeEnv = liftIO $ do
  targets            <- newTVarIO []
  clipboard          <- clipboardGet selectionClipboard
  return Env { cTargets            = targets
             , cClipboard          = clipboard
             }

checkClipboard = do
  cb <- clipboard
  io $ \run ->
    clipboardRequestTargets cb $ \targets -> run $ do
      updateClipboardTargets targets
      liftIO $ timeoutAdd (run checkClipboard) 250
      return ()
  return False
