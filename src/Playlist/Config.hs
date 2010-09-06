-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Jul. 2010
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

module Playlist.Config
  ( initPlaylistConfig
  , showPlaylistConfigDialog
  ) where

import System.IO.Unsafe

import Graphics.UI.Gtk

import UI
import Editor
import Context
import Compound
import Playlist.Format
import Playlist.Format.Config


data Config
  = Config { cDialog :: EditorDialog FormatView }

initPlaylistConfig = do
  dialog <- unsafeInterleaveIO $ makeEditorDialog
            [(stockApply, ResponseApply)]
            makePlaylistFormatView $ \m -> do
    let outerw = outer m
    windowSetTitle outerw "Configure playlist"
    windowSetDefaultSize outerw 500 400
  return $ augmentContext
    Config { cDialog = dialog }

showPlaylistConfigDialog =
  runEditorDialog (cDialog context) getFormatDefs putFormatDefs False window
