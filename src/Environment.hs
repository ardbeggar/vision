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
  ( EnvironmentEnvClass
  , initEnvironment
  , homeDir
  , dataDir
  , uiFilePath
  , xmmsPath
  , maybeGetEnv
  ) where

import qualified System.Environment as SE
import System.FilePath
import Control.Applicative

import Env

import Paths_vision


data Environment
  = Environment { eHomeDir  :: Maybe FilePath
                , eDataDir  :: FilePath
                , eXMMSPath :: Maybe String }

class (EnvClass Environment c) => EnvironmentEnvClass c where {}
instance (EnvClass Environment c) => EnvironmentEnvClass c

homeDir   = eHomeDir getEnv
dataDir   = eDataDir getEnv
xmmsPath  = eXMMSPath getEnv


initEnvironment = do
  homeDir  <- maybeGetEnv "HOME"
  dataDir  <- getDataDir
  xmmsPath <- maybeGetEnv "XMMS_PATH"
  return $ makeEnv Environment { eHomeDir  = homeDir
                               , eDataDir  = dataDir
                               , eXMMSPath = xmmsPath
                               }

maybeGetEnv var =
  (Just <$> SE.getEnv var) `catch` \_ -> return Nothing

uiFilePath name = dataDir </> "ui" </> name <.> "glade"

