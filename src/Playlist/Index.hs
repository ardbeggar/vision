-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Jun. 2010
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

module Playlist.Index
  ( initIndex
  , getInfo
  , addToIndex
  ) where

import Control.Concurrent.MVar
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Graphics.UI.Gtk hiding (add)

import Medialib
import Env
import Handler
import Playlist.Model


data IndexEntry
  = IENone
  | IERetrieving
  | IEReady Stamp MediaInfo

data Index
  = Index { iTable :: MVar (IntMap (IndexEntry, [TreeRowReference])) }

index = iTable getEnv


initIndex = do
  env <- initEnv
  let ?env = env

  onMediaInfo . add . ever $ handleInfo

  return ?env


initEnv = do
  table <- newMVar IntMap.empty
  return $ augmentEnv
    Index { iTable = table }

handleInfo (id, stamp, info) = do
  let id' = fromIntegral id
  modifyMVar_ index $ \ix ->
    case IntMap.lookup id' ix of
      Just (entry, list) -> updateIndex ix id' stamp info entry list
      Nothing            -> return ix

updateIndex ix id stamp info old list =
  if upd
  then do
    mapM_ touch list
    return $ IntMap.insert id (new, list) ix
  else
    return ix
  where new = IEReady stamp info
        upd = case old of
          IEReady oldStamp _ -> stamp /= oldStamp
          _                  -> True

touch ref = do
  path <- treeRowReferenceGetPath ref
  case path of
    [n] -> touchPlaylist n
    _   -> return ()

getInfo id force = do
  let id' = fromIntegral id
  modifyMVar index $ \ix ->
    case IntMap.lookup id' ix of
      Just (IEReady _ info, _) ->
        return (ix, Just info)
      Just (IENone, list) | force -> do
        requestInfo id
        return (IntMap.insert id'(IERetrieving, list) ix, Nothing)
      _ ->
        return (ix, Nothing)

addToIndex id n =
  modifyMVar_ index $ \ix -> do
    Just ref <- treeRowReferenceNew playlistStore [n]
    return $ IntMap.insertWith iw (fromIntegral id) (IENone, [ref]) ix
  where iw (_, [new]) (entry, old) = (entry, new : old)