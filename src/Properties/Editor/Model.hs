-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

module Properties.Editor.Model
  ( initEditorModel
  , store
  ) where

import Control.Concurrent.MVar
import Control.Monad.State hiding (State)
import Control.Applicative

import Data.Maybe
import Data.List

import Graphics.UI.Gtk hiding (add)

import Context
import Config
import Utils
import Handler
import Properties.Property
import Properties.Model


data State
  = State { sPos :: Int }

makeState =
  State { sPos = 0 }

data Model
  = Model { mState :: MVar (Maybe State)
          , mStore :: ListStore Property
          }

state = mState context
store = mStore context

setupState =
  modifyMVar_ state . const . return $ Just makeState

withState f =
  modifyMVar state $ \(Just s) -> do
    (a, s') <- runStateT f s
    return (Just s', a)

initEditorModel = do
  context <- initContext
  let ?context = context

  cid <- store `on` rowDeleted $ const saveConfig
  onProperties . add . ever . const $
    withSignalBlocked cid updateProperties
  loadConfig

  return ?context

initContext = do
  state <- newMVar Nothing
  store <- listStoreNew []
  return $ augmentContext
    Model { mState = state
          , mStore = store
          }

loadConfig = do
  cfg <- mapM property =<< config configFile []
  populateStore cfg

saveConfig = do
  names <- map propName <$> listStoreToList store
  writeConfig configFile names
  return ()

updateProperties = do
  cur <- mapM (property . propName) =<< listStoreToList store
  listStoreClear store
  populateStore cur

populateStore cur = do
  all <- propertyList
  mapM_ (listStoreAppend store) $
    unionBy (eqBy propName) (catMaybes cur) all

configFile = "property-editor.conf"