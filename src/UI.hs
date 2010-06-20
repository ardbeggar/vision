-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

module UI
  ( initUI
  , window
  ) where

import Graphics.UI.Gtk

import Env
import Environment


data UI
  = UI { uWindow :: Window }

window = uWindow getEnv


initUI ::
  (?env :: e1, EnvironmentEnvClass e1) =>
     FilePath
  -> (EnvType UI e1 -> Builder -> IO b)
  -> IO b
initUI file cont = do
  builder <- builderNew
  builderAddFromFile builder $ uiFilePath file
  window <- builderGetObject builder castToWindow "window"

  cont (augmentEnv UI { uWindow = window }) builder
