-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Sep. 2010
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

module About
  ( initAbout
  , showAbout
  ) where

import Control.Concurrent.MVar

import Data.Version

import Graphics.UI.Gtk

import Paths_vision
import Context
import Utils


data About
  = About { aAbout :: MVar (Maybe AboutDialog) }


initAbout = do
  about <- newMVar Nothing
  return $ augmentContext
    About { aAbout = about }

showAbout window =
  tryModifyMVar_ (aAbout context) $ \maybeAbout ->
    case maybeAbout of
      Just about -> do
        windowPresent about
        return $ Just about
      Nothing    -> do
        about <- aboutDialogNew
        windowSetModal about False
        windowSetTransientFor about window
        aboutDialogSetName about "Vision"
        aboutDialogSetVersion about $ showVersion version
        aboutDialogSetComments about "An XMMS2 client."
        aboutDialogSetCopyright about "Copyright © 2010 Oleg Belozeorov."
        aboutDialogSetLicense about $ Just license
        aboutDialogSetWrapLicense about True
        aboutDialogSetAuthors about authors

        about `onResponse` \_ ->
          modifyMVar_ (aAbout context) . const $ do
            widgetDestroy about
            return Nothing

        widgetShowAll about
        return $ Just about


authors =
  [ "Oleg Belozeorov <upwawet@gmail.com>" ]

license =
  "Vision is free software; you can redistribute it and/or \
  \modify it under the terms of the GNU General Public License \
  \as published by the Free Software Foundation; either version 3 \
  \of the License, or (at your option) any later version.\n\n\
  \Vision is distributed in the hope that it will be useful, but \
  \WITHOUT ANY WARRANTY; without even the implied warranty of \
  \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. \
  \See the GNU General Public License for more details.\n\n\
  \You should have received a copy of the GNU General Public \
  \License along with Vision; if not, write to the Free Software \
  \Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, \
  \MA 02110-1301, USA."

