-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

module Location
  ( browseLocation
  ) where

import Control.Monad.Trans
import Control.Monad.EnvIO

import Graphics.UI.Gtk

import Builder
import UIEnvIO
import Environment

import Location.Model
import Location.View
import Location.DnD
import Location.Control
import Location.UI


browseLocation order maybeURL = do
  let f = browseLocation

  flip runEnvIO () $ runBuilder $ do
    addFromFile $ gladeFilePath "location-browser"

    context <- liftIO $ initModel order
    let ?context = context

    context <- initView
    let ?context = context

    runUI $ do
      setupUI f
      liftIO $ setupDnD

      window <- window
      liftIO $ widgetShowAll window

      case maybeURL of
        Just url -> loadLocation $ Go url
        Nothing  -> openLocation
