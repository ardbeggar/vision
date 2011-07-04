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
import Control.Monad.ReaderX
import Control.Monad.ToIO

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Data.Typeable

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Context
import XMMS


data Model
  = Model { store :: ListStore (Maybe String) }
    deriving (Typeable)

data Ix = Ix deriving (Typeable)
instance Index Ix where getVal = Ix

modelEnv :: (Ix, Model)
modelEnv = undefined

initModel = do
  model <- mkModel
  runEnvT (Ix, model) $ runIn modelEnv $> setupModel
  addEnv Ix model

mkModel = liftIO $ do
  store <- listStoreNewDND [] Nothing Nothing
  return Model { store = store }

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
    colls <- result
    liftIO $ run $ do
      clearStore
      fillStore colls

clearStore = do
  store <- asksx Ix store
  liftIO $ listStoreClear store

fillStore colls = do
  store <- asksx Ix store
  liftIO $ do
    listStoreAppend store Nothing
    mapM_ (listStoreAppend store . Just) colls
