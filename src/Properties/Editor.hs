-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

module Properties.Editor
  ( initPropertyEditor
  , showPropertyEditor
  ) where

import Environment (WithEnvironment)
import Registry (WithRegistry)
import XMMS (WithXMMS)

import Properties.Model (WithModel)

import Properties.Editor.Model (withModel)
import Properties.Editor.View (withView)
import Properties.Editor.UI (initUI, showPropertyEditor)

initPropertyEditor :: (WithEnvironment, WithRegistry, WithXMMS, WithModel) => IO ()
initPropertyEditor = withModel $ withView initUI

