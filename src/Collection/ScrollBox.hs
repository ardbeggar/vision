-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 5 Jul. 2011
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

module Collection.ScrollBox
  ( ScrollBox (..)
  , mkScrollBox
  , scrollBoxAdd
  ) where

import Graphics.UI.Gtk


data ScrollBox
  = SB { sBox      :: HBox
       , sViewport :: Viewport
       , sHAdj     :: Adjustment
       , sVAdj     :: Adjustment
       }

mkScrollBox = do
  box      <- hBoxNew False 5
  hAdj     <- adjustmentNew 0 0 0 0 0 0
  vAdj     <- adjustmentNew 0 0 0 0 0 0
  viewport <- viewportNew hAdj vAdj
  containerAdd viewport box
  return SB { sBox      = box
            , sViewport = viewport
            , sHAdj     = hAdj
            , sVAdj     = vAdj
            }

scrollBoxAdd sb widget =
  boxPackStart (sBox sb) widget PackNatural 0
