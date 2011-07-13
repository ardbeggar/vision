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

{-# LANGUAGE DeriveDataTypeable #-}

module Collection.List.Model
  ( initModel
  , store
  , modelEnv
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ToIO
import Control.Monad.EnvIO
import Control.Monad.W

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Typeable
import Data.Env

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Context
import XMMS


data Model
  = Model { mStore :: ListStore (Maybe (String, Coll)) }
    deriving (Typeable)

data Ix = Ix deriving (Typeable)

modelEnv :: Extract Ix Model
modelEnv = Extract

initModel = do
  model <- mkModel
  runIn (mkEnv Ix model) $> setupModel
  addEnv Ix model

mkModel = liftIO $ do
  store <- listStoreNewDND [] Nothing Nothing
  return Model { mStore = store }

setupModel = io $ \run -> do
  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    postGUISync $ run clearStore
    when conn $ do
      broadcastCollectionChanged xmms >>* do
        change <- result
        when (namespace change == "Collections") $
          liftIO $ run listCollections
        persist
      run listCollections

listCollections = io $ \run ->
  collList xmms "Collections" >>* do
    names <- result
    liftIO $ run $ do
      clearStore
      fillStore names

store = envsx Ix mStore

clearStore = do
  store <- store
  liftIO $ listStoreClear store

fillStore names = do
  store <- store
  liftIO $ do
    listStoreAppend store Nothing
    forM_ names $ \name ->
      collGet xmms name "Collections" >>* do
        coll <- result
        liftIO $ listStoreAppend store $ Just (name, coll)
