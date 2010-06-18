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

module Environment
  ( initEnvironment
  , homeDir
  , xmmsPath
  , maybeGetEnv
  ) where

import qualified System.Environment as SE
import Control.Applicative

import Env


data Environment
  = Environment { eHomeDir   :: Maybe String
                , eXMMSPath  :: Maybe String }

homeDir   = eHomeDir getEnv
xmmsPath  = eXMMSPath getEnv


initEnvironment = do
  homeDir  <- maybeGetEnv "HOME"
  xmmsPath <- maybeGetEnv "XMMS_PATH"
  return $ makeEnv Environment { eHomeDir   = homeDir
                               , eXMMSPath  = xmmsPath }


maybeGetEnv var =
  (Just <$> SE.getEnv var) `catch` \_ -> return Nothing
