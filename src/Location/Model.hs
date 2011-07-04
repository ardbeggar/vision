-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Location.Model
  ( initModel
  , Item (..)
  , makeItem
  , locationStore
  , sortModel
  , getCurrentLocation
  , location
  , LocationChange (..)
  , updateLocation
  , canGo
  , itemByIter
  , itemByPath
  , setSortOrder
  , getSortOrder
  ) where

import Control.Concurrent.MVar

import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Applicative

import Data.List
import Data.Ord

import Graphics.UI.Gtk

import XMMS2.Client

import Context
import Utils


data Location
  = Location { lCurrent :: String
             , lBack    :: [String]
             , lForward :: [String]
             }

makeLocation =
  Location { lCurrent = ""
           , lBack    = []
           , lForward = []
           }

data LocationChange
  = Back
  | Forward
  | Go String
  | Refresh
  | Up

changeLocation Back l =
  case lBack l of
    (u : us) ->
      (l { lCurrent = u
         , lBack    = us
         , lForward = lCurrent l : lForward l
         }, Just u)
    _ ->
      (l, Nothing)
changeLocation Forward l =
  case lForward l of
    (u : us) ->
      (l { lCurrent = u
         , lBack    = lCurrent l : lBack l
         , lForward = us
         }, Just u)
    _ ->
      (l, Nothing)
changeLocation (Go u) l =
  (l { lCurrent = u
     , lBack    = case lCurrent l of
         [] -> lBack l
         u' -> u' : lBack l
     , lForward = []
     }, Just u)
changeLocation Refresh l =
  (l, Just $ lCurrent l)
changeLocation Up l =
  let u = reverse . dropWhile (/= '/') . tail . reverse $ lCurrent l
  in changeLocation (Go u) l


data State
  = State { sOrder :: SortType }

makeState order =
  State { sOrder = order }

data Item
  = Item { iName  :: String
         , iPath  :: String
         , iIsDir :: Bool
         }

makeItem x =
  Item { iName  = name
       , iPath  = path
       , iIsDir = entryIsDir x
       }
  where name = last $ split path
        path = decodeURL $ entryPath x

compareItems SortDescending a b
  | iIsDir a == iIsDir b = comparing iName b a
  | iIsDir a             = LT
  | iIsDir b             = GT
compareItems SortAscending a b
  | iIsDir a == iIsDir b = comparing iName a b
  | iIsDir a             = LT
  | iIsDir b             = GT


data Model
  = Model { mState    :: MVar State
          , mStore    :: ListStore Item
          , mSort     :: TypedTreeModelSort Item
          , mLocation :: TGVar Location
          }

locationStore = mStore context
sortModel = mSort context
location = mLocation context
state = mState context

getCurrentLocation = lCurrent <$> readTGVarIO location

updateLocation change = atomically $ do
  old <- readTGVar location
  let (new, u) = changeLocation change old
  writeTGVar location new
  return u

canGo = atomically $ do
  Location { lCurrent = c
           , lBack    = b
           , lForward = f
           } <- readTGVar location
  return (not $ null b,
          not $ null f,
          not $ null c || isSuffixOf "//" c,
          not $ null c)


initModel order = do
  context <- initContext order
  let ?context = context

  setSortOrder order

  return ?context


initContext order = do
  state    <- newMVar $ makeState order
  store    <- listStoreNewDND [] Nothing Nothing
  sort     <- treeModelSortNewWithModel store
  location <- atomically $ newTGVar makeLocation
  return $ augmentContext
    Model { mState    = state
          , mStore    = store
          , mSort     = sort
          , mLocation = location
          }

split [] =  []
split s  =
  let (p, s') = break (== '/') s in
  p : case s' of
        []      -> []
        (_:s'') -> split s''

itemByIter' iter = do
  [n]   <- treeModelGetPath locationStore iter
  listStoreGetValue locationStore n

itemByIter iter = do
  citer <- treeModelSortConvertIterToChildIter sortModel iter
  itemByIter' citer

itemByPath path = do
  [n] <- treeModelSortConvertPathToChildPath sortModel path
  listStoreGetValue locationStore n

compareIters order a b =
  compareItems order <$> itemByIter' a <*> itemByIter' b

getSortOrder =
  withMVar state $ return . sOrder

setSortOrder order = do
  modifyMVar_ state $ \s ->
    return s { sOrder = order }
  treeSortableSetDefaultSortFunc sortModel $ Just $ compareIters order
