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

{-# LANGUAGE UndecidableInstances #-}

module Environment
  ( EnvironmentContextClass
  , initEnvironment
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

import Context hiding (getEnv)

import Paths_vision


data Environment
  = Environment { eHomeDir  :: Maybe FilePath
                , eDataDir  :: FilePath
                , eXMMSPath :: Maybe String }

class (ContextClass Environment c) => EnvironmentContextClass c where {}
instance (ContextClass Environment c) => EnvironmentContextClass c

homeDir   = eHomeDir context
dataDir   = eDataDir context
xmmsPath  = eXMMSPath context


initEnvironment = do
  homeDir  <- maybeGetEnv "HOME"
  dataDir  <- getDataDir
  xmmsPath <- maybeGetEnv "XMMS_PATH"
  return $ makeContext Environment { eHomeDir  = homeDir
                               , eDataDir  = dataDir
                               , eXMMSPath = xmmsPath
                               }

maybeGetEnv var =
  (Just <$> getEnv var) `catch` \_ -> return Nothing

uiFilePath name = dataDir </> "ui" </> name <.> "xml"
gladeFilePath name = dataDir </> "ui" </> name <.> "glade"

