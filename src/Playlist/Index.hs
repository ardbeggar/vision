-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Jun. 2010
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

{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Playlist.Index
  ( withIndex
  , getInfo
  , addToIndex
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import Control.Applicative
import Control.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Graphics.UI.Gtk hiding (add)

import Medialib
import Playlist.Model
import Playlist.Format


data IndexEntry
  = IENone
  | IERetrieving RequestPriority
  | IEReady Stamp MediaInfo TrackInfo

data Index
  = Index { _table :: MVar (IntMap (IndexEntry, [TreeRowReference])) }

index = _table ?_Playlist_Index


newtype Wrap a = Wrap { unWrap :: (?_Playlist_Index :: Index) => a }

withIndex    = withIndex' . Wrap
withIndex' w = do
  index <- mkIndex
  let ?_Playlist_Index = index

  (atomically $ dupTChan mediaInfoChan) >>= forkIO . handleInfo
  fW <- atomically $ newTWatch formatsGeneration 0
  forkIO $ forever $ do
    void $ atomically $ watch fW
    postGUISync handleFormats

  unWrap w


mkIndex = do
  table <- newMVar IntMap.empty
  return Index { _table = table }

handleInfo chan = forever $ do
  (id, stamp, info) <- atomically $ readTChan chan
  let id' = fromIntegral id
  modifyMVar_ index $ \ix ->
    case IntMap.lookup id' ix of
      Just (entry, list) -> updateIndex ix id' stamp info entry list
      Nothing            -> return ix

handleFormats =
  modifyMVar_ index $ \ix ->
    IntMap.fromList <$> mapM update (IntMap.toList ix)
  where update (i, (IEReady s m _, l)) = do
          t <- makeTrackInfo m
          mapM_ touch l
          return (i, (IEReady s m t, l))
        update other =
          return other

updateIndex ix id stamp info old list =
  if upd
  then do
    ti <- makeTrackInfo info
    idleAdd (mapM_ touch list >> return False) priorityDefault
    return $ IntMap.insert id (IEReady stamp info ti, list) ix
  else
    return ix
  where upd = case old of
          IEReady oldStamp _ _ -> stamp /= oldStamp
          _                    -> True

touch ref = do
  path <- treeRowReferenceGetPath ref
  case path of
    [n] -> touchPlaylist n
    _   -> return ()

getInfo id prio = do
  let id' = fromIntegral id
  modifyMVar index $ \ix ->
    case IntMap.lookup id' ix of
      Just (IEReady _ _ info, _) ->
        return (ix, Just info)
      Just (IENone, list) -> do
        requestInfo prio id
        return (IntMap.insert id' (IERetrieving prio, list) ix, Nothing)
      Just (IERetrieving old, list) | old > prio -> do
        requestInfo prio id
        return (IntMap.insert id' (IERetrieving prio, list) ix, Nothing)
      _ ->
        return (ix, Nothing)

addToIndex id n =
  modifyMVar_ index $ \ix -> do
    Just ref <- treeRowReferenceNew playlistStore [n]
    return $ IntMap.insertWith iw (fromIntegral id) (IENone, [ref]) ix
  where iw (_, [new]) (entry, old) = (entry, new : old)
