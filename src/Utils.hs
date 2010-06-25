-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Jun. 2010
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

{-# LANGUAGE ScopedTypeVariables #-}

module Utils
  ( HandlerMVar
  , makeHandlerMVar
  , onHandler
  , mapFst
  , decodeURL
  , trd
  , trim
  , bracket_
  , catchResult
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO hiding (Handler)
import Control.Applicative
import Control.Concurrent.MVar
import Data.Char
import Codec.Binary.UTF8.String

import XMMS2.Client

import Handler


type HandlerMVar a = MVar (Handler IO a)

makeHandlerMVar = newMVar make

onHandler = modifyMVar

mapFst f (a, b) = (f a, b)

decodeURL = decodeString . decodeURL'
decodeURL' []         = []
decodeURL' ('+' : cs) = ' ' : decodeURL' cs
decodeURL' ('%' : cs) = let (c, cs') = decodeByte cs in c : decodeURL' cs'
decodeURL' (c : cs)   = c : decodeURL' cs

decodeByte (d1 : d0 : cs)
  | isHexDigit d1 && isHexDigit d0 = (chr $ (digitToInt d1) * 16 + (digitToInt d0), cs)
decodeByte _ = error "invalid URL"

trd (_, _, c) = c

trim = f . f where f = reverse . dropWhile isSpace

bracket_ f g = bracket f (const g) . const

catchResult def conv =
  (conv <$> result) `catch` \(_ :: XMMSException) -> return def
