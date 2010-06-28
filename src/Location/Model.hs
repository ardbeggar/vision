-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
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

{-# LANGUAGE ScopedTypeVariables #-}

module Location.Model
  ( initModel
  , Item (..)
  , locationStore
  , loadLocation
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO

import Control.Concurrent.MVar
import Control.Monad.Trans

import Graphics.UI.Gtk

import XMMS2.Client

import Env
import XMMS
import Utils


data State
  = State { sLocation :: Maybe String }

makeState =
  State { sLocation = Nothing }

data Item
  = Item { iName  :: String
         , iPath  :: String
         , iIsDir :: Bool
         }

makeItem x =
  Item { iName  = name
       , iPath  = path
       , iIsDir = entryIsDir x }
  where name = last $ split $ path
        path = decodeURL $ entryPath x

data Model
  = Model { mState :: MVar State
          , mStore :: ListStore Item
          }

locationStore = mStore getEnv


initModel = do
  env <- initEnv
  let ?env = env

  return ?env


initEnv = do
  state <- newMVar makeState
  store <- listStoreNewDND [] Nothing Nothing
  return $ augmentEnv
    Model { mState = state
          , mStore = store
          }

loadLocation = do
  let url = "file:///srv/share/music"
  listStoreClear locationStore
  xformMediaBrowse xmms url >>* handleBrowse url

handleBrowse url = do
  handleBrowse' `catch` \(_ :: XMMSException) ->
    liftIO $ putStrLn $ "error loading " ++ url
  return False
  where handleBrowse' = do
          r <- result
          liftIO $ mapM_ (listStoreAppend locationStore . makeItem) r

split [] =  []
split s  =
  let (p, s') = break (== '/') s in
  p : case s' of
        []      -> []
        (_:s'') -> split s''

