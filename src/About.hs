-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Sep. 2010
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

{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}

module About
  ( initAbout
  , showAbout
  ) where

import Control.Concurrent.MVar

import Data.Version
import Data.Env
import Data.Typeable

import Graphics.UI.Gtk

import Paths_vision
import Registry
import Utils


data Ix = Ix deriving (Typeable)

data About
  = About { _about :: MVar (Maybe AboutDialog) }
    deriving (Typeable)

initAbout :: WithRegistry => IO ()
initAbout = do
  about <- newMVar Nothing
  addEnv Ix About { _about = about }

showAbout :: (WithRegistry, WindowClass w) => w -> IO ()
showAbout window = do
  Just (Env e) <- getEnv (Extract :: Extract Ix About)
  tryModifyMVar_ (_about e) $ \maybeAbout ->
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
        aboutDialogSetCopyright about "Copyright © 2010-2012 Oleg Belozeorov."
        aboutDialogSetLicense about $ Just license
        aboutDialogSetWrapLicense about True
        aboutDialogSetAuthors about authors

        about `onResponse` \_ ->
          modifyMVar_ (_about e) . const $ do
            widgetDestroy about
            return Nothing

        widgetShowAll about
        return $ Just about

authors :: [String]
authors =
  [ "Oleg Belozeorov <upwawet@gmail.com>" ]

license :: String
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

