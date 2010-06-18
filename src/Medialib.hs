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
  , onMediaInfo
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe

import XMMS2.Client
import XMMS2.Client.Bindings (propdictToDict)

import Utils
import Env
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
  = MLib { mCache       :: MVar Cache
         , mOnMediaInfo :: HandlerMVar (Stamp, MediaInfo)
         }

cache = mCache getEnv


onMediaInfo = onHandler $ mOnMediaInfo getEnv

initMedialib = do
  env <- initEnv
  let ?env = env

  onConnected . add . ever . const $
    broadcastMedialibEntryChanged xmms >>* do
      id <- result
      let id' = fromIntegral id
      liftIO $ do
        withMVar cache $ \cache ->
          when (isJust . IntMap.lookup id' $ cEntries cache) $
            medialibGetInfo xmms id >>* handleInfo id'
      return True

  return ?env


initEnv = do
  cache       <- newMVar emptyCache
  onMediaInfo <- makeHandlerMVar
  return $ augmentEnv
    MLib { mCache       = cache
         , mOnMediaInfo = onMediaInfo }

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
    onMediaInfo $ invoke (stamp, info)
  return False
