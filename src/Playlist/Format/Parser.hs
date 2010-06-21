-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 23 Feb. 2010
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

module Playlist.Format.Parser
  ( parseFormat
  ) where

import Control.Applicative hiding (many, optional, (<|>))

import Text.Parsec
import Text.Parsec.String

import Playlist.Format.Format (Fe (..))


parseFormat = parse format ""

format :: Parser [Fe String]
format = do
  fes <- many fe
  eof
  return fes

fe :: Parser (Fe String)
fe = feP <|> feO <|> feL

feP :: Parser (Fe String)
feP = FeP <$> between (char '{') (char '}') (many1 (noneOf "{}"))

feL :: Parser (Fe String)
feL = FeL <$> many1 (esc <|> noneOf "{}[]")

feO :: Parser (Fe String)
feO = FeO <$> between (char '[') (char ']') (many fe)

esc = char '\\' >> anyChar
