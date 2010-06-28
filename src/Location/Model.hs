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

module Location.Model
  ( initModel
  , Item (..)
  , makeItem
  , locationStore
  , getCurrentLocation
  , setCurrentLocation
  ) where

import Control.Concurrent.MVar

import Graphics.UI.Gtk

import XMMS2.Client

import Env
import Utils


data State
  = State { sLocation :: String }

makeState =
  State { sLocation = "" }

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
state = mState getEnv

getCurrentLocation =
  withMVar state $ return . sLocation
setCurrentLocation location =
  modifyMVar_ state $ \state ->
    return state { sLocation = location }


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

split [] =  []
split s  =
  let (p, s') = break (== '/') s in
  p : case s' of
        []      -> []
        (_:s'') -> split s''


