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
  , getInfo
  , retrieveProperties
  , mediaInfoChan
  ) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import qualified Data.IntSet as IntSet

import XMMS2.Client
import XMMS2.Client.Bindings (propdictToDict)

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
         , mMediaInfoChan :: TChan (MediaId, Stamp, MediaInfo)
         , mReqChan       :: Chan MediaId
         }

cache         = mCache context
mediaInfoChan = mMediaInfoChan context
reqChan       = mReqChan context

initMedialib = do
  context <- initContext
  let ?context = context

  forkIO infoReqJob

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
        writeChan reqChan id
        return cache { cEntries = IntMap.insert id' CERetrieving $ cEntries cache }
      Just (CEReady s i) -> do
        atomically $ do
          writeTChan mediaInfoChan (id, s, i)
          void $ readTChan mediaInfoChan
        return cache
      _ ->
        return cache

initContext = do
  cache         <- newMVar emptyCache
  mediaInfoChan <- atomically $ newTChan
  reqChan       <- newChan
  return $ augmentContext
    MLib { mCache         = cache
         , mMediaInfoChan = mediaInfoChan
         , mReqChan       = reqChan
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
    atomically $ do
      writeTChan mediaInfoChan (fromIntegral id, stamp, info)
      void $ readTChan mediaInfoChan

getInfo id =
  withMVar cache $ \cache ->
    return $ case IntMap.lookup (fromIntegral id) $ cEntries cache of
      Just (CEReady _ info) -> Just info
      _                     -> Nothing

retrieveProperties ids f = do
  let ids'       = IntSet.fromList $ map fromIntegral ids
      len        = IntSet.size ids'
      step       = len `div` 100

  chan <- atomically $ dupTChan mediaInfoChan
  let handler st@(ctr, todo, ready) = do
        (id, _, info) <- atomically $ readTChan chan
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
            when (step == 0 || ctr' `mod` step == 0) $
              f . Left $ fromIntegral ctr' / fromIntegral len
            handler (ctr', todo', (id, info) : ready)
          else
          handler st

  tid <- forkIOUnmasked $ handler (0, ids', [])
  forkIO $ mapM_ (requestInfo . fromIntegral) $ IntSet.toList ids'

  return $ killThread tid

infoReqJob = do
  ids <- getChanContents reqChan
  infoReqJob' ids

infoReqJob' list = do
  let (h, t) = splitAt 30 list
  forM_ h $ \id ->
    medialibGetInfo xmms id >>* handleInfo (fromIntegral id)
  threadDelay 1
  infoReqJob' t
