-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection.Model
  ( initModel
  , collStore
  ) where

import Control.Concurrent.MVar

import Graphics.UI.Gtk

import XMMS2.Client

import Context
import Collection.Common


data State
  = State { sCurColl :: Coll
          , sCurName :: String
          }

makeState =
  State { sCurColl = universe
        , sCurName = ""
        }

data Model
  = Model { mState :: MVar State
          , mStore :: ListStore MediaId
          }

collStore = mStore context


initModel = do
  context <- initContext
  let ?context = context

  return ?context


initContext = do
  state <- newMVar makeState
  store <- listStoreNewDND [] Nothing Nothing
  return $ augmentContext
    Model { mState = state
          , mStore = store
          }
