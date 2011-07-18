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

{-# LANGUAGE RankNTypes #-}

module Environment
  ( withEnvironment
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

import Paths_vision


data Environment
  = Environment { _homeDir  :: Maybe FilePath
                , _dataDir  :: FilePath
                , _XMMSPath :: Maybe String
                }

homeDir   = _homeDir ?_Environment
dataDir   = _dataDir ?_Environment
xmmsPath  = _XMMSPath ?_Environment

newtype Wrap a = Wrap { unWrap :: (?_Environment :: Environment) => a }

withEnvironment    = withEnvironment' . Wrap
withEnvironment' w = do
  homeDir  <- maybeGetEnv "HOME"
  dataDir  <- getDataDir
  xmmsPath <- maybeGetEnv "XMMS_PATH"
  let ?_Environment =
        Environment { _homeDir  = homeDir
                    , _dataDir  = dataDir
                    , _XMMSPath = xmmsPath
                    }
  unWrap w

maybeGetEnv var =
  (Just <$> getEnv var) `catch` \_ -> return Nothing

uiFilePath name = dataDir </> "ui" </> name <.> "xml"
gladeFilePath name = dataDir </> "ui" </> name <.> "glade"

