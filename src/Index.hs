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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Index
  ( Index
  , makeIndex
  , getInfo
  , addToIndex
  ) where

import Control.Concurrent.MVar
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import Medialib hiding (getInfo)
import Handler


data IndexEntry i
  = IENone
  | IERetrieving
  | IEReady Stamp MediaInfo i

data Index i
  = Index { iTable :: MVar (IntMap (IndexEntry i, [TreeRowReference]))
          , iStore :: ListStore MediaId
          , iConv  :: MediaInfo -> IO i
          }

makeIndex store conv = do
  table <- newMVar IntMap.empty
  let index = Index { iTable = table
                    , iStore = store
                    , iConv  = conv
                    }

  onMediaInfo . add . ever $ handleInfo index

  return index

handleInfo index (id, stamp, info) = do
  let id' = fromIntegral id
  modifyMVar_ (iTable index) $ \ix ->
    case IntMap.lookup id' ix of
      Just (entry, list) ->
        updateIndex index ix id' stamp info entry list
      Nothing            ->
        return ix

updateIndex index ix id stamp info old list =
  if upd
  then do
    ti <- (iConv index) info
    mapM_ (touch index) list
    return $ IntMap.insert id (IEReady stamp info ti, list) ix
  else
    return ix
  where upd = case old of
          IEReady oldStamp _ _ -> stamp /= oldStamp
          _                    -> True

touch index ref = do
  path <- treeRowReferenceGetPath ref
  case path of
    [n] -> do
      Just iter <- treeModelGetIter (iStore index) [n]
      treeModelRowChanged (iStore index) [n] iter
    _   ->
      return ()

getInfo index id force = do
  let id' = fromIntegral id
  modifyMVar (iTable index) $ \ix ->
    case IntMap.lookup id' ix of
      Just (IEReady _ _ info, _) ->
        return (ix, Just info)
      Just (IENone, list) | force -> do
        requestInfo id
        return (IntMap.insert id'(IERetrieving, list) ix, Nothing)
      _ ->
        return (ix, Nothing)

addToIndex index id n =
  modifyMVar_ (iTable index) $ \ix -> do
    Just ref <- treeRowReferenceNew (iStore index) [n]
    return $ IntMap.insertWith iw (fromIntegral id) (IENone, [ref]) ix
  where iw (_, [new]) (entry, old) = (entry, new : old)
