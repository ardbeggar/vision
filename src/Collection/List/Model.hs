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

{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

module Collection.List.Model
  ( initModel
  , withModel
  , store
  , modelEnv
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.EnvIO

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Maybe
import Data.Typeable

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Registry
import XMMS


data Model
  = Model { _store :: ListStore (Maybe (String, Coll)) }
    deriving (Typeable)

data Ix = Ix deriving (Typeable)

modelEnv :: Extract Ix Model
modelEnv = Extract

initModel = do
  model <- mkModel
  let ?collectionListModel = model
  setupModel
  addEnv Ix model

newtype Wrap a = Wrap { unWrap :: (?collectionListModel :: Model) => a }

withModel    = withModel' . Wrap
withModel' w = do
  Env model <- liftIO $ do
    maybeME <- getEnv modelEnv
    case maybeME of
      Just me -> return me
      Nothing -> do
        initModel
        fromJust <$> getEnv modelEnv
  let ?collectionListModel = model in unWrap w


mkModel = do
  store <- listStoreNewDND [] Nothing Nothing
  return Model { _store = store }

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
        persist
      listCollections

listCollections =
  collList xmms "Collections" >>* do
    names <- result
    liftIO $ do
      clearStore
      fillStore names

store = _store ?collectionListModel

clearStore =
  listStoreClear store

fillStore names = do
  listStoreAppend store Nothing
  forM_ names $ \name ->
    collGet xmms name "Collections" >>* do
      coll <- result
      liftIO $ listStoreAppend store $ Just (name, coll)
