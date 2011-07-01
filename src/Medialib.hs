-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
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

module Medialib
  ( Stamp
  , MediaInfo
  , RequestPriority (..)
  , initMedialib
  , requestInfo
  , retrieveProperties
  , mediaInfoChan
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Monad (when, void, forever)
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.PSQueue (PSQ, Binding (..))
import qualified Data.PSQueue as PSQ

import XMMS2.Client
import XMMS2.Client.Bindings (propdictToDict)

import Context
import XMMS
import Utils


type Stamp = Int
type MediaInfo = Map String Property

data RequestPriority
  = Current
  | Visible
  | Search
  | Changed
  | Background
    deriving (Eq, Ord)

data CacheEntry
  = CEReady Stamp MediaInfo
  | CERetrieving RequestPriority

data Cache
  = Cache { cEntries   :: IntMap CacheEntry
          , cNextStamp :: Stamp
          }

emptyCache =
  Cache { cEntries   = IntMap.empty
        , cNextStamp = 1
        }


data MLib
  = MLib { mCache         :: TVar (Maybe Cache)
         , mMediaInfoChan :: TChan (MediaId, Stamp, MediaInfo)
         , mReqQ          :: TVar (PSQ MediaId RequestPriority)
         }

cache         = mCache context
reqQ          = mReqQ context
mediaInfoChan = mMediaInfoChan context

initMedialib = do
  context <- initContext
  let ?context = context

  xcW <- atomically $ newTGWatch connectedV
  let mon xc
        | xc = do
          atomically $ writeTVar cache $ Just emptyCache
          broadcastMedialibEntryChanged xmms >>* do
            id <- result
            let id' = fromIntegral id
            liftIO $ atomically $ do
              cc <- readTVar cache
              withJust cc $ \cc ->
                when (IntMap.member id' $ cEntries cc) $ do
                  r <- readTVar reqQ
                  writeTVar reqQ $ PSQ.insert id Changed r
            persist
          rt <- forkIO infoReqJob
          xc <- atomically $ watch xcW
          killThread rt
          atomically $ do
            writeTVar cache Nothing
            writeTVar reqQ PSQ.empty
          mon xc
        | otherwise = do
          xc <- atomically $ watch xcW
          mon xc
  forkIO $ mon False

  return ?context

requestInfo prio id = atomically $ do
  cc <- readTVar cache
  withJust cc $ \cc ->
    let id'     = fromIntegral id
        entries = cEntries cc
    in case IntMap.lookup id' entries of
      Nothing -> do
        r <- readTVar reqQ
        writeTVar reqQ $ PSQ.insert id prio r
        writeTVar cache $ Just
          cc { cEntries = IntMap.insert id' (CERetrieving prio) entries }
      Just (CERetrieving old) | old > prio -> do
        r <- readTVar reqQ
        writeTVar reqQ $ PSQ.update (const $ Just prio) id r
        writeTVar cache $ Just
          cc { cEntries = IntMap.insert id' (CERetrieving prio) entries }
      Just (CEReady s i) -> do
        writeTChan mediaInfoChan (id, s, i)
        void $ readTChan mediaInfoChan
      _ -> return ()

initContext = do
  cache         <- newTVarIO Nothing
  mediaInfoChan <- newTChanIO
  reqQ          <- newTVarIO PSQ.empty
  return $ augmentContext
    MLib { mCache         = cache
         , mMediaInfoChan = mediaInfoChan
         , mReqQ          = reqQ
         }

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
  forkIO $ mapM_ (requestInfo Background . fromIntegral) $ IntSet.toList ids'

  return $ killThread tid

infoReqJob = do
  tv <- newTVarIO 0
  forever $ do
    id <- atomically $ do
      c <- readTVar tv
      when (c > 100) retry
      r <- readTVar reqQ
      case PSQ.minView r of
        Nothing               -> retry
        Just (id :-> _, rest) -> do
          writeTVar reqQ rest
          writeTVar tv $ c + 1
          return id
    medialibGetInfo xmms id >>* do
      rawv <- resultRawValue
      liftIO $ do
        info  <- valueGet =<< propdictToDict rawv []
        stamp <- atomically $ do
          c <- readTVar tv
          writeTVar tv $ c - 1
          cc <- readTVar cache
          case cc of
            Just cc -> do
              let stamp   = cNextStamp cc
                  entries = cEntries cc
                  entry   = CEReady stamp info
              writeTVar cache $ Just
                Cache { cEntries   = IntMap.insert (fromIntegral id) entry entries
                      , cNextStamp = succ stamp
                      }
              return $ Just stamp
            Nothing -> return Nothing
        withJust stamp $ \stamp -> atomically $ do
          writeTChan mediaInfoChan (id, stamp, info)
          void $ readTChan mediaInfoChan
