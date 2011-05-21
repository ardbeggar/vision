-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
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

module Medialib
  ( Stamp
  , MediaInfo
  , initMedialib
  , requestInfo
  , onMediaInfo
  , getInfo
  , retrieveProperties
  , mediaInfoChan
  ) where

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import qualified Data.IntSet as IntSet

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client
import XMMS2.Client.Bindings (propdictToDict)

import Utils
import Context
import XMMS
import Handler


type Stamp = Int
type MediaInfo = Map String Property


data CacheEntry
  = CEReady Stamp MediaInfo
  | CERetrieving

data Cache
  = Cache { cEntries   :: IntMap CacheEntry
          , cNextStamp :: Stamp
          }

emptyCache =
  Cache { cEntries   = IntMap.empty
        , cNextStamp = 1
        }


data MLib
  = MLib { mCache         :: MVar Cache
         , mOnMediaInfo   :: HandlerMVar (MediaId, Stamp, MediaInfo)
         , mMediaInfoChan :: Chan (MediaId, Stamp, MediaInfo)
         }

cache         = mCache context
onMediaInfo   = onHandler $ mOnMediaInfo context
mediaInfoChan = mMediaInfoChan context

initMedialib = do
  context <- initContext
  let ?context = context

  onServerConnectionAdd . ever $ \conn ->
    if conn
    then broadcastMedialibEntryChanged xmms >>* do
      id <- result
      let id' = fromIntegral id
      liftIO $ withMVar cache $ \cache ->
        when (isJust . IntMap.lookup id' $ cEntries cache) $
          medialibGetInfo xmms id >>* handleInfo id'
      persist
    else
      modifyMVar_ cache $ \cache ->
        return cache { cEntries = IntMap.empty }

  return ?context

requestInfo id =
  modifyMVar_ cache $ \cache ->
    let id'     = fromIntegral id
        entries = cEntries cache in
    case IntMap.lookup id' entries of
      Nothing -> do
        medialibGetInfo xmms id >>* handleInfo id'
        return cache { cEntries = IntMap.insert id' CERetrieving $ cEntries cache }
      Just (CEReady s i) -> do
        writeChan mediaInfoChan (id, s, i)
        idleAdd (onMediaInfo (invoke (id, s, i)) >> return False) priorityHighIdle
        return cache
      _ ->
        return cache

initContext = do
  cache         <- newMVar emptyCache
  onMediaInfo   <- makeHandlerMVar
  mediaInfoChan <- newChan
  forkIO $ forever $ void $ readChan mediaInfoChan
  return $ augmentContext
    MLib { mCache         = cache
         , mOnMediaInfo   = onMediaInfo
         , mMediaInfoChan = mediaInfoChan
         }

handleInfo id = do
  rawv <- resultRawValue
  liftIO $ do
    info  <- valueGet =<< propdictToDict rawv []
    stamp <- modifyMVar cache $ \cache ->
      let stamp   = cNextStamp cache
          entries = cEntries cache
          entry   = CEReady stamp info in
      return (Cache { cEntries   = IntMap.insert id entry entries
                    , cNextStamp = succ stamp }, stamp)
    writeChan mediaInfoChan (fromIntegral id, stamp, info)
    onMediaInfo $ invoke (fromIntegral id, stamp, info)

getInfo id =
  withMVar cache $ \cache ->
    return $ case IntMap.lookup (fromIntegral id) $ cEntries cache of
      Just (CEReady _ info) -> Just info
      _                     -> Nothing

retrieveProperties ids f = do
  let ids'       = IntSet.fromList $ map fromIntegral ids
      len        = IntSet.size ids'
      step       = len `div` 100

  chan <- dupChan mediaInfoChan
  let handler st@(ctr, todo, ready) = do
        (id, _, info) <- readChan chan
        let id' = fromIntegral id
        if IntSet.member id' todo
          then do
          let todo' = IntSet.delete id' todo
          if IntSet.null todo'
            then do
            f . Right $ reverse ((id, info) : ready)
            return ()
            else do
            let ctr' = ctr + 1
            when (step == 0 || ctr' `mod` step == 0) $ do
              print ctr'
              f . Left $ fromIntegral ctr' / fromIntegral len
            handler (ctr', todo', (id, info) : ready)
          else
          handler st

  tid <- forkIOUnmasked $ handler (0, ids', [])
  forkIO $ mapM_ (requestInfo . fromIntegral) $ IntSet.toList ids'

  return $ killThread tid
