-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

module Playtime
  ( initPlaytime
  ) where

import Prelude hiding (catch)

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO

import qualified Data.Map as Map

import XMMS2.Client

import XMMS
import Handler
import Medialib
import Env
import Utils


data State
  = State { sCurrentId :: Maybe MediaId }

data Playtime
  = Playtime { pState :: MVar State }

state = pState getEnv

getCurrentId    = withMVar state $ return . sCurrentId
setCurrentId id = modifyMVar_ state $ \s -> return  s { sCurrentId = id }

initPlaytime = do
  env <- initEnv
  let ?env = env

  onConnected . add . ever . const $ do
    playbackCurrentId xmms >>* handleCurrentId False
    broadcastPlaybackCurrentId xmms >>* handleCurrentId True

  onDisconnected . add . ever . const $ resetState

  onMediaInfo . add . ever $ handleInfo

  return ?env


initEnv = do
  state <- newMVar makeState
  return $ augmentEnv
    Playtime { pState = state }

makeState =
  State { sCurrentId = Nothing }

resetState =
  modifyMVar_ state $ const $ return makeState

handleCurrentId ret = do
  cid <- result
  liftIO $ do
    setCurrentId $
      if cid == 0
      then Nothing
      else Just cid
    requestInfo cid
  return ret

handleInfo (id, _, info) = do
  cid <- getCurrentId
  when (cid == Just id) $ do
    case Map.lookup "duration" info of
      Just (PropInt32 d) -> putStrLn $ "duration: " ++ show d
      _                  -> return ()
