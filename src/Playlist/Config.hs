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
  ( showPlaylistConfigDialog
  ) where

import Graphics.UI.Gtk

import UI
import Utils
import Playlist.Format.Config


showPlaylistConfigDialog = do
  windowGroup <- windowGroupNew

  dialog <- dialogNew
  windowGroupAddWindow windowGroup dialog
  windowSetTransientFor dialog window
  windowSetModal dialog False
  windowSetTitle dialog "Configure playlist"
  windowSetDefaultSize dialog 500 400
  dialogSetHasSeparator dialog False

  dialogAddButton   dialog "gtk-apply"  ResponseApply
  dialogAddButton   dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok"     ResponseOk

  upper <- dialogGetUpper dialog
  fview <- makePlaylistFormatView windowGroup (return ())
  boxPackStartDefaults upper $ formatViewBox fview

  widgetShowAll dialog
