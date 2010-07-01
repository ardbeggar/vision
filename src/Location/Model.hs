-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
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

module Location.Model
  ( initModel
  , Item (..)
  , makeItem
  , locationStore
  , getCurrentLocation
  , onLocation
  , LocationChange (..)
  , updateLocation
  , canGo
  ) where

import Control.Concurrent.MVar

import Data.List

import Graphics.UI.Gtk

import XMMS2.Client

import Env
import Utils
import Handler


data Location
  = Location { lCurrent :: String
             , lBack    :: [String]
             , lForward :: [String]
             }

makeLocation =
  Location { lCurrent = ""
           , lBack    = []
           , lForward = []
           }

data LocationChange
  = Back
  | Forward
  | Go String
  | Refresh
  | Up

changeLocation Back l =
  case lBack l of
    (u : us) ->
      (l { lCurrent = u
         , lBack    = us
         , lForward = lCurrent l : lForward l
         }, Just u)
    _ ->
      (l, Nothing)
changeLocation Forward l =
  case lForward l of
    (u : us) ->
      (l { lCurrent = u
         , lBack    = lCurrent l : lBack l
         , lForward = us
         }, Just u)
    _ ->
      (l, Nothing)
changeLocation (Go u) l =
  (l { lCurrent = u
     , lBack    = case lCurrent l of
         [] -> lBack l
         u' -> u' : lBack l
     , lForward = []
     }, Just u)
changeLocation Refresh l =
  (l, Just $ lCurrent l)
changeLocation Up l =
  let u = reverse . dropWhile (/= '/') . tail . reverse $ lCurrent l
  in changeLocation (Go u) l


data State
  = State { sLocation :: Location }

makeState =
  State { sLocation = makeLocation }

data Item
  = Item { iName  :: String
         , iPath  :: String
         , iIsDir :: Bool
         }

makeItem x =
  Item { iName  = name
       , iPath  = path
       , iIsDir = entryIsDir x
       }
  where name = last $ split $ path
        path = decodeURL $ entryPath x

data Model
  = Model { mState      :: MVar State
          , mStore      :: ListStore Item
          , mOnLocation :: HandlerMVar ()
          }

locationStore = mStore getEnv
onLocation = onHandler $ mOnLocation getEnv
state = mState getEnv

getCurrentLocation =
  withMVar state $ return . lCurrent . sLocation

updateLocation change = do
  r <- modifyMVar state $ \s -> do
    let (l, u) = changeLocation change $ sLocation s
    return (s { sLocation = l }, u)
  onLocation $ invoke ()
  return r

canGo = withMVar state $ \s ->
  let Location { lCurrent = c
               , lBack    = b
               , lForward = f
               } = sLocation s
  in return (not $ null b,
             not $ null f,
             not $ null c || isSuffixOf "//" c,
             not $ null c)


initModel = do
  env <- initEnv
  let ?env = env

  return ?env


initEnv = do
  state      <- newMVar makeState
  store      <- listStoreNewDND [] Nothing Nothing
  onLocation <- makeHandlerMVar
  return $ augmentEnv
    Model { mState      = state
          , mStore      = store
          , mOnLocation = onLocation
          }

split [] =  []
split s  =
  let (p, s') = break (== '/') s in
  p : case s' of
        []      -> []
        (_:s'') -> split s''

