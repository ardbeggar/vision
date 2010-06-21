-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Jun. 2010
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

module Config
  ( config
  , writeConfig
  ) where

import Data.Maybe

import Text.JSON

import System.FilePath
import System.Directory
import System.IO

import Environment


config name defl = do
  withFile (configFileName name) ReadMode config' `catch` \_ -> return defl
  where config' h = do
          c <- hGetContents h
          case decodeStrict c of
            Ok r    -> return r
            Error _ -> readIO c -- try to read old format

writeConfig name cont =
  writeConfig' >> return True `catch` \_ -> return False
  where writeConfig' = do
          createDirectoryIfMissing True configBaseDir
          writeFile (configFileName name) $ encodeStrict $ showJSON cont


configFileName name = joinPath [configBaseDir, name]

configBaseDir = joinPath [fromJust homeDir, ".vision"]
