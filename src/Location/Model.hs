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
  , UpdateLocation (..)
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


data State
  = State { sLocation :: String
          , sBack     :: [String]
          , sForward  :: [String]
          }

makeState =
  State { sLocation = ""
        , sBack     = []
        , sForward  = []
        }

data Item
  = Item { iName  :: String
         , iPath  :: String
         , iIsDir :: Bool
         }

makeItem x =
  Item { iName  = name
       , iPath  = path
       , iIsDir = entryIsDir x }
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
  withMVar state $ return . sLocation


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

data UpdateLocation
  = Back
  | Forward
  | Go String
  | Refresh
  | Up

updateLocation location = do
  r <- modifyMVar state $ \s -> return $
    case location of
      Back ->
        case sBack s of
          (u : us) ->
            (s { sLocation = u
               , sBack = us
               , sForward = sLocation s : sForward s
               }
            , Just u)
          _ ->
            (s, Nothing)
      Forward ->
        case sForward s of
          (u : us) ->
            (s { sLocation = u
               , sBack = sLocation s : sBack s
               , sForward = us
               }
            , Just u)
          _ ->
            (s, Nothing)
      Go u ->
        (s { sLocation = u
           , sBack = case sLocation s of
             [] -> sBack s
             u' -> u' : sBack s
           , sForward = []
           }
        , Just u)
      Refresh ->
        (s, Just $ sLocation s)
      Up ->
        let u  = sLocation s
            u' = reverse . dropWhile (/= '/') . tail $ reverse u in
        (s { sLocation = u'
           , sBack = case u of
             [] -> sBack s
             _  -> u : sBack s
           , sForward = []
           }
        , Just u')
  onLocation $ invoke ()
  return r

canGo = withMVar state $ \State { sLocation = l
                                , sBack     = b
                                , sForward  = f
                                }  ->
  return (not $ null b,
          not $ null f,
          not $ null l || isSuffixOf "//" l,
          not $ null l)
