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
  ) where

import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import XMMS2.Client
--import XMMS2.Client.Bindings (propdictToDict)

import Env


type Stamp = Int
type MediaInfo = Map String Property

data CacheEntry
  = CEReady Stamp MediaInfo
  | CERetrieving

type Cache = IntMap CacheEntry

data MLib
  = MLib { mCache     :: Cache
         , mNextStamp :: Stamp
         }

withCache f = f $ mCache getEnv


initMedialib = do
  env <- initEnv
  let ?env = env

  return ?env


initEnv = do
  return $ augmentEnv
    MLib { mCache     = IntMap.empty
         , mNextStamp = 0 }
