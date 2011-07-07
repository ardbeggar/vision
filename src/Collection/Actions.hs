-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

module Collection.Actions
  ( ActionBackend (..)
  , emptyAB
  ) where

import Graphics.UI.Gtk

import XMMS2.Client


data ActionBackend
  = AB { aWithColl  :: (Coll -> IO ()) -> IO ()
       , aWithNames :: ([String] -> IO ()) -> IO ()
       , aSelection :: Maybe TreeSelection
       }

emptyAB =
  AB { aWithColl  = const $ return ()
     , aWithNames = const $ return ()
     , aSelection = Nothing
     }
