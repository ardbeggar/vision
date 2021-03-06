-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
--
--  Copyright (C) 2010, 2011, 2012 Oleg Belozeorov
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

{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}

module Medialib
  ( Stamp
  , MediaInfo
  , RequestPriority (..)
  , initMedialib
  , WithMedialib
  , withMedialib
  , requestInfo
  , retrieveProperties
  , mediaInfoChan
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Monad (when, forever)
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.PSQueue (PSQ, Binding (..))
import qualified Data.PSQueue as PSQ
import Data.Env
import Data.Typeable

import XMMS2.Client
import XMMS2.Client.Bindings (propdictToDict)

import Registry
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

emptyCache :: Cache
emptyCache =
  Cache { cEntries   = IntMap.empty
        , cNextStamp = 1
        }

data Ix = Ix deriving (Typeable)

data MLib
  = MLib { _cache         :: TVar (Maybe Cache)
         , _mediaInfoChan :: TChan (MediaId, Stamp, MediaInfo)
         , _reqQ          :: TVar (PSQ MediaId RequestPriority)
         }
    deriving (Typeable)

type WithMedialib = ?_Medialib :: MLib

cache :: WithMedialib => TVar (Maybe Cache)
cache = _cache ?_Medialib

reqQ :: WithMedialib => TVar (PSQ MediaId RequestPriority)
reqQ = _reqQ ?_Medialib

mediaInfoChan :: WithMedialib => TChan (MediaId, Stamp, MediaInfo)
mediaInfoChan = _mediaInfoChan ?_Medialib

initMedialib :: WithRegistry => IO ()
initMedialib = withXMMS $ do
  mlib <- mkMLib
  addEnv Ix mlib
  let ?_Medialib = mlib

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
            return True
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

  return ()

withMedialib :: WithRegistry => (WithMedialib => IO a) -> IO a
withMedialib func = do
  Just (Env mlib) <- getEnv (Extract :: Extract Ix MLib)
  let ?_Medialib = mlib
  func

requestInfo :: WithMedialib => RequestPriority -> MediaId -> IO ()
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
      Just (CEReady s i) ->
        writeBroadcastTChan mediaInfoChan (id, s, i)
      _ -> return ()

mkMLib :: IO MLib
mkMLib = do
  cache         <- newTVarIO Nothing
  mediaInfoChan <- newTChanIO
  reqQ          <- newTVarIO PSQ.empty
  return MLib { _cache         = cache
              , _mediaInfoChan = mediaInfoChan
              , _reqQ          = reqQ
              }

retrieveProperties ::WithMedialib => [MediaId] -> (Either Double [(MediaId, MediaInfo)] -> IO ()) -> IO (IO ())
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

  tid <- forkIO $ handler (0, ids', [])
  forkIO $ mapM_ (requestInfo Background . fromIntegral) $ IntSet.toList ids'

  return $ killThread tid

infoReqJob :: (WithXMMS, WithMedialib) => IO a
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
        withJust stamp $ \stamp -> atomically $
          writeBroadcastTChan mediaInfoChan (id, stamp, info)
