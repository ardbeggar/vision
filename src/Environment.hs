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

{-# LANGUAGE Rank2Types #-}

module Environment
  ( WithEnvironment
  , withEnvironment
  , homeDir
  , dataDir
  , uiFilePath
  , gladeFilePath
  , xmmsPath
  , maybeGetEnv
  ) where

import System.Environment
import System.FilePath
import Control.Applicative

import Prelude hiding (catch)
import Control.Exception

import Paths_vision


data Environment
  = Environment { _homeDir  :: Maybe FilePath
                , _dataDir  :: FilePath
                , _XMMSPath :: Maybe String
                }

type WithEnvironment = ?_Environment :: Environment

homeDir :: WithEnvironment => Maybe FilePath
homeDir = _homeDir ?_Environment

dataDir :: WithEnvironment => FilePath
dataDir = _dataDir ?_Environment

uiFilePath :: WithEnvironment => String -> FilePath
uiFilePath name = dataDir </> "ui" </> name <.> "xml"

gladeFilePath :: WithEnvironment => String -> FilePath
gladeFilePath name = dataDir </> "ui" </> name <.> "glade"

xmmsPath :: WithEnvironment => Maybe String
xmmsPath = _XMMSPath ?_Environment

withEnvironment :: (WithEnvironment => IO a) -> IO a
withEnvironment func = do
  homeDir  <- maybeGetEnv "HOME"
  dataDir  <- getDataDir
  xmmsPath <- maybeGetEnv "XMMS_PATH"
  let ?_Environment =
        Environment { _homeDir  = homeDir
                    , _dataDir  = dataDir
                    , _XMMSPath = xmmsPath
                    }
  func

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv var =
  (Just <$> getEnv var) `catch` \(_ :: SomeException) -> return Nothing
