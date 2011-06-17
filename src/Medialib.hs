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
  , retrieveProperties
  , mediaInfoChan
  ) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad (when, void, forever)
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Foldable (forM_)

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
  = MLib { mCache         :: TVar Cache
         , mMediaInfoChan :: TChan (MediaId, Stamp, MediaInfo)
         , mReq           :: TVar (Seq MediaId)
         }

cache         = mCache context
mediaInfoChan = mMediaInfoChan context

initMedialib = do
  context <- initContext
  let ?context = context

  forkIO infoReqJob
  setupConn

  return ?context

setupConn = do
  onServerConnectionAdd . ever $ \conn ->
    if conn
    then broadcastMedialibEntryChanged xmms >>* do
      id <- result
      let id' = fromIntegral id
      liftIO $ atomically $ do
        cc <- readTVar cache
        when (IntMap.member id' $ cEntries cc) $ do
          r <- readTVar (mReq context)
          writeTVar (mReq context) (id <| r)
      persist
    else atomically $ writeTVar cache emptyCache

requestInfo id = atomically $ do
  cc <- readTVar cache
  let id'     = fromIntegral id
      entries = cEntries cc
  case IntMap.lookup id' entries of
    Nothing -> do
      r <- readTVar (mReq context)
      writeTVar (mReq context) (r |> id)
      writeTVar cache $ cc { cEntries = IntMap.insert id' CERetrieving entries }
    Just (CEReady s i) -> do
      writeTChan mediaInfoChan (id, s, i)
      void $ readTChan mediaInfoChan
    _ -> return ()

initContext = do
  cache         <- newTVarIO emptyCache
  mediaInfoChan <- newTChanIO
  req           <- newTVarIO Seq.empty
  return $ augmentContext
    MLib { mCache         = cache
         , mMediaInfoChan = mediaInfoChan
         , mReq           = req
         }

handleInfo id = do
  rawv <- resultRawValue
  liftIO $ do
    info  <- valueGet =<< propdictToDict rawv []
    stamp <- atomically $ do
      cc <- readTVar cache
      let stamp   = cNextStamp cc
          entries = cEntries cc
          entry   = CEReady stamp info
      writeTVar cache $
        Cache { cEntries   = IntMap.insert id entry entries
              , cNextStamp = succ stamp
              }
      return stamp
    atomically $ do
      writeTChan mediaInfoChan (fromIntegral id, stamp, info)
      void $ readTChan mediaInfoChan

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

infoReqJob = forever $ do
  ids <- atomically $ do
    r <- readTVar (mReq context)
    if Seq.null r
      then retry
      else do
      let (h, t) = Seq.splitAt 30 r
      writeTVar (mReq context) t
      return h
  forM_ ids $ \id ->
    medialibGetInfo xmms id >>* handleInfo (fromIntegral id)
  threadDelay 1


