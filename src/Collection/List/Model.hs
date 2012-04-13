-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jul. 2010
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

module Collection.List.Model
  ( initModel
  , WithModel
  , withModel
  , store
  , index
  , modelEnv
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Env
import Data.IORef

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Registry
import XMMS


data Model
  = Model { _store :: ListStore (Maybe (String, Coll))
          , _index :: IORef (Map (Maybe String) TreeRowReference)
          }
    deriving (Typeable)

type WithModel = ?_Collection_List_Model :: Model

store :: WithModel => ListStore (Maybe (String, Coll))
store = _store ?_Collection_List_Model

index :: WithModel => IORef (Map (Maybe String) TreeRowReference)
index = _index ?_Collection_List_Model

data Ix = Ix deriving (Typeable)

modelEnv :: Extract Ix Model
modelEnv = Extract

initModel :: (WithRegistry, WithXMMS) => IO ()
initModel = do
  model <- mkModel
  let ?_Collection_List_Model = model
  setupModel
  addEnv Ix model

withModel :: (WithRegistry, WithXMMS) => (WithModel => IO a) -> IO a
withModel func = do
  Env model <- do
    maybeME <- getEnv modelEnv
    case maybeME of
      Just me -> return me
      Nothing -> do
        initModel
        fromJust <$> getEnv modelEnv
  let ?_Collection_List_Model = model
  func

mkModel :: IO Model
mkModel = do
  store <- listStoreNewDND [] Nothing Nothing
  index <- newIORef Map.empty
  return Model { _store = store
               , _index = index
               }

setupModel :: (WithXMMS, WithModel) => IO ThreadId
setupModel = do
  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    postGUISync clearStore
    when conn $ do
      broadcastCollectionChanged xmms >>* do
        change <- result
        when (namespace change == "Collections") $
          liftIO listCollections
        return True
      listCollections

listCollections :: (WithXMMS, WithModel) => IO ()
listCollections =
  collList xmms "Collections" >>* do
    names <- result
    liftIO $ do
      clearStore
      fillStore names

clearStore :: WithModel => IO ()
clearStore = do
  writeIORef index Map.empty
  listStoreClear store

addRow :: WithModel => Maybe (String, Coll) -> IO ()
addRow row = do
  n <- listStoreAppend store row
  Just r <- treeRowReferenceNew store [n]
  modifyIORef index $ Map.insert (fst <$> row) r

fillStore :: (WithXMMS, WithModel) => [String] -> IO ()
fillStore names = do
  addRow Nothing
  forM_ names $ \name ->
    collGet xmms name "Collections" >>* do
      coll <- result
      liftIO $ addRow $ Just (name, coll)
