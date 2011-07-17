-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Jul. 2010
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

{-# LANGUAGE RankNTypes #-}

module Playlist.Config
  ( withPlaylistConfig
  , showPlaylistConfigDialog
  ) where

import System.IO.Unsafe

import Graphics.UI.Gtk

import UI
import Editor
import Compound
import Playlist.Format
import Playlist.Format.Config


data Config
  = Config { _dialog :: EditorDialog FormatView }

newtype Wrap a = Wrap { unWrap :: (?_Playlist_Config :: Config) => a }

withPlaylistConfig    = withPlaylistConfig' . Wrap
withPlaylistConfig' w = do
  dialog <- unsafeInterleaveIO $ makeEditorDialog
            [(stockApply, ResponseApply)]
            makePlaylistFormatView $ \m -> do
    let outerw = outer m
    windowSetTitle outerw "Configure playlist"
    windowSetDefaultSize outerw 500 400

  let ?_Playlist_Config = Config { _dialog = dialog }
  unWrap w

showPlaylistConfigDialog =
  runEditorDialog (_dialog ?_Playlist_Config)
  getFormatDefs putFormatDefs False window
