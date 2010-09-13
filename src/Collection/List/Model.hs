-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jul. 2010
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

module Collection.List.Model
  ( initListModel
  , listStore
  ) where

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Context
import XMMS
import Handler


data Model
  = Model { mStore :: ListStore (Maybe String) }

listStore = mStore context


initListModel = do
  context <- initContext
  let ?context = context

  onServerConnection . add . ever $ \conn ->
    if conn
    then do
      broadcastCollectionChanged xmms >>* do
        change <- result
        when (namespace change == "Collections") $
          -- TODO: use the change info instead of repopulating the model.
          liftIO listCollections
        persist
      listCollections
    else
      listStoreClear listStore

  return ?context

initContext = do
  store <- listStoreNewDND [] Nothing Nothing
  return $ augmentContext
    Model { mStore = store }

listCollections =
  collList xmms "Collections" >>* do
    colls <- result
    liftIO $ do
      listStoreClear listStore
      listStoreAppend listStore Nothing
      mapM_ (listStoreAppend listStore . Just) colls
