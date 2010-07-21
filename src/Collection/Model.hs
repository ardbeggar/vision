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
  , getInfo
  , populateModel
  , getCurName
  , getCurColl
  , setCurColl
  , getFilter
  , setFilter
  , resetModel
  , getLoaded
  , setLoaded
  ) where

import Control.Concurrent.MVar

import Graphics.UI.Gtk

import XMMS2.Client

import Context
import Index hiding (getInfo)
import qualified Index as Index
import Medialib hiding (getInfo)
import Collection.Common


data State
  = State { sCurColl :: Coll
          , sCurName :: String
          , sFilter  :: String
          , sLoaded  :: Bool
          }

makeState =
  State { sCurColl = universe
        , sCurName = ""
        , sFilter  = ""
        , sLoaded  = False
        }

data Model
  = Model { mState :: MVar State
          , mStore :: ListStore MediaId
          , mIndex :: Index MediaInfo
          }

state = mState context

collStore = mStore context
collIndex = mIndex context

getCurName =
  withMVar state $ return . sCurName

getCurColl =
  withMVar state $ \s -> do
    case sFilter s of
      []   -> return $ sCurColl s
      text -> do
        fil <- collParse text
        int <- collNew TypeIntersection
        collAddOperand int $ sCurColl s
        collAddOperand int fil
        return int

setCurColl coll name =
  modifyMVar_ state $ \s ->
    return s { sCurColl = coll
             , sCurName = name
             }

getFilter = withMVar state $ return . sFilter

setFilter text =
  modifyMVar_ state $ \s ->
    return s { sFilter = text }

getLoaded =
  withMVar state $ return . sLoaded

setLoaded loaded =
  modifyMVar_ state $ \s ->
    return s { sLoaded = loaded }

getInfo = Index.getInfo (mIndex context)


initModel = do
  context <- initContext
  let ?context = context

  return ?context


initContext = do
  state <- newMVar makeState
  store <- listStoreNewDND [] Nothing Nothing
  index <- makeIndex store return
  return $ augmentContext
    Model { mState = state
          , mStore = store
          , mIndex = index
          }

populateModel ids = do
  clearIndex collIndex
  listStoreClear collStore
  mapM_ addOne ids
  where addOne id = do
          n <- listStoreAppend collStore id
          addToIndex collIndex id n

resetModel = do
  listStoreClear collStore
  modifyMVar_ state . const $ return makeState
