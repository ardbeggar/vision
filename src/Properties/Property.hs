-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 2 Mar. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.Property
  ( PropertyType (..)
  , Property (..)
  , readValue
  , showValue
  , lookup
  , builtinProperties
  , builtinPropertyMap
  ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad

import Data.Maybe
import qualified Data.Map as Map

import Text.Printf
import Text.JSON

import qualified XMMS2.Client.Types as X

import Utils


data PropertyType
  = PropertyString
  | PropertyInt
    deriving (Read, Show, Enum)

instance JSON PropertyType where
  showJSON PropertyInt    = showJSON "int"
  showJSON PropertyString = showJSON "string"
  readJSON (JSString s) =
    case fromJSString s of
      "int"    -> return PropertyInt
      "string" -> return PropertyString
      _        -> fail "Unable to read PropertyType"
  readJSON _ = fail "Unable to read PropertyType"


type PropertyShow = X.Property -> String
type PropertyRead = String -> IO X.Property

data Property
  = Property { propName      :: String
             , propKey       :: String
             , propType      :: PropertyType
             , propCustom    :: Bool
             , propReadOnly  :: Bool
             , propReadValue :: Maybe PropertyRead
             , propShowValue :: Maybe PropertyShow }

instance Read Property where
  readsPrec x s = map (mapFst mk) (readsPrec x s)

instance Show Property where
  show p = show (propName p, propKey p, propType p, propReadOnly p)

instance JSON Property where
  showJSON p = showJSON (propName p, propKey p, propType p, propReadOnly p)
  readJSON v = mk <$> readJSON v `mplus` fail "Unable to read Property"

mk (pn, pk, pt, pr) =
  case Map.lookup pk builtinPropertyMap of
    Just prop ->
      prop { propName   = pn
           , propCustom = True }
    _ ->
      Property { propName      = pn
               , propKey       = pk
               , propType      = pt
               , propCustom    = True
               , propReadOnly  = pr
               , propReadValue = Nothing
               , propShowValue = Nothing }




readValue p s  = liftM Just read `catch` \_ -> return Nothing
  where read   = fromMaybe rdef (propReadValue p) s
        rdef s = case (propType p) of
                   PropertyInt    -> X.PropInt32 <$> readIO s
                   PropertyString -> return $ X.PropString s

showValue p v = fromMaybe sdef (propShowValue p) v
  where sdef (X.PropInt32  n) = show n
        sdef (X.PropString s) = s


lookup p m = do
  res <- showValue p <$> Map.lookup (propKey p) m
  when (null res) mzero
  return res


builtinPropertyMap =
  Map.fromList $ map (\p -> (propKey p, p)) builtinProperties

builtinProperties =
  map (\(pn, pk, pt, ro, pr, ps) ->
         Property { propName      = pn
                  , propKey       = pk
                  , propType      = pt
                  , propCustom    = False
                  , propReadOnly  = ro
                  , propReadValue = pr
                  , propShowValue = ps })
      [ ("Artist", "artist",
         PropertyString, False, Nothing, Nothing)
      , ("Album", "album",
         PropertyString, False, Nothing, Nothing)
      , ("Date", "date",
         PropertyString, False, Nothing, Nothing)
      , ("Track", "tracknr",
         PropertyInt, False, Nothing, Nothing)
      , ("Title", "title",
         PropertyString, False, Nothing, Nothing)
      , ("Comment", "comment",
         PropertyString, False, Nothing, Nothing)
      , ("Duration", "duration",
         PropertyInt, True, Nothing, Just showDuration)
      , ("URL", "url",
         PropertyString, True,  Nothing, Just showURL)
      , ("Composer", "composer",
         PropertyString, False, Nothing, Nothing)
      , ("Work", "work",
         PropertyString, False, Nothing, Nothing)
      , ("Catalog", "catalog",
         PropertyString, False, Nothing, Nothing)
      , ("Movement", "movement",
         PropertyString, False, Nothing, Nothing)
      , ("Performer", "performer",
         PropertyString, False, Nothing, Nothing)
      , ("Conductor", "conductor",
         PropertyString, False, Nothing, Nothing)
      , ("Orchestra", "orchestra",
         PropertyString, False, Nothing, Nothing)
      , ("Chorus master", "chorus_master",
         PropertyString, False, Nothing, Nothing)
      , ("Chorus", "chorus",
         PropertyString, False, Nothing, Nothing)
      , ("Channel", "channel",
         PropertyString, False, Nothing, Nothing) ]


showDuration (X.PropInt32  n) =
  if h == 0 then mss else show h ++ (':' : mss)
  where d   = n `div` 1000
        h   = d `div` 3600
        m   = (d - (h * 3600)) `div` 60
        s   = d - h * 3600 - m * 60
        mss = printf "%02d:%02d" m s

showURL (X.PropString u) = decodeURL u
