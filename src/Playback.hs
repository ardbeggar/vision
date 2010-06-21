-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

module Playback
  ( initPlayback
  ) where

import Control.Concurrent.MVar

import XMMS2.Client

import Env


data State
  = State { sCurrentTrack :: Maybe (Int, String)
          , sStatus       :: Maybe PlaybackStatus
          }

data Playback
  = Playback { pState :: MVar State }


initPlayback = do
  env <- initEnv
  let ?env = env

  return ?env


initEnv = do
  state <- newMVar makeState
  return $ augmentEnv
    Playback { pState = state }

makeState =
  State { sCurrentTrack = Nothing
        , sStatus       = Nothing
        }

