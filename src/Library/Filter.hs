-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 31 Mar. 2012
--
--  Copyright (C) 2012 Oleg Belozeorov
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

module Library.Filter
  (
  ) where

import Prelude hiding (filter)

import Control.Monad
import Control.Applicative ((<$>))

import Text.Parsec
import Text.Parsec.String

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char (isSpace)

data Props = Props deriving (Show)
data Colls = Colls deriving (Show)

newtype SetOf a = SetOf { unSetOf :: Set String } deriving (Show)

data PropSel
  = CSany (SetOf Props)
  | CSall (SetOf Props)
  deriving (Show)

test_propSel = parseTest propSel

propSel :: Parser PropSel
propSel = try anyOf <|> allOf

anyOf :: Parser PropSel
anyOf = do
  lexeme1 $ string "any"
  CSany <$> setOf

allOf :: Parser PropSel
allOf = do
  lexeme1 $ string "all"
  CSall <$> setOf

setOf :: Parser (SetOf a)
setOf = do
  lexeme1 $ string "of"
  SetOf <$> names

lexeme :: Parser a -> Parser a
lexeme p = do
  a <- p
  spaces
  return a

lexeme1 :: Parser a -> Parser a
lexeme1 p = do
  a <- p
  spaces1
  return a

spaces1 :: Parser ()
spaces1 = skipMany1 space

comma :: Parser ()
comma = lexeme $ void $ char ','

commaSep1 :: Parser a -> Parser [a]
commaSep1 = flip sepBy1 comma

name :: Parser String
name = lexeme $ many1 $ satisfy nameChar

nameChar :: Char -> Bool
nameChar ','           = False
nameChar c | isSpace c = False
nameChar _             = True

names :: Parser (Set String)
names = Set.fromList <$> commaSep1 name

value :: Parser String
value = name

data Filter
  = Fmatch PropSel String
  deriving (Show)

test_filter = parseTest filter

filter :: Parser Filter
filter = match

match :: Parser Filter
match = do
  sel <- propSel
  lexeme1 $ string "match"
  val <- value
  return $ Fmatch sel val
