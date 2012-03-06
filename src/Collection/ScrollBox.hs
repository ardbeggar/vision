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

import Control.Monad.Fix
import Control.Monad.Trans

import Graphics.UI.Gtk

import Compound


data ScrollBox
  = SB { sBox      :: HBox
       , sViewport :: Viewport
       }

mkScrollBox = do
  box      <- hBoxNew False 0
  hAdj     <- adjustmentNew 0 0 0 0 0 0
  vAdj     <- adjustmentNew 0 0 0 0 0 0
  viewport <- viewportNew hAdj vAdj
  viewportSetShadowType viewport ShadowNone
  containerSetBorderWidth box 0
  ph <- hBoxNew False 0
  ph `set` [ widgetWidthRequest := 5 ]
  boxPackStart box ph PackNatural 0
  containerAdd viewport box
  return SB { sBox      = box
            , sViewport = viewport
            }

scrollBoxAdd sb widget = do
  vs <- vSeparatorNew
  eb <- eventBoxNew
  containerAdd eb vs
  eventBoxSetVisibleWindow eb True
  widgetSetSizeRequest eb 9 (-1)
  widgetShowAll eb
  setupResize eb widget
  boxPackStart (sBox sb) widget PackNatural 0
  boxPackStart (sBox sb) eb PackNatural 0

setupResize eb widget = do
  eb `after` realize $ do
    cs <- cursorNew SbHDoubleArrow
    dw <- widgetGetDrawWindow eb
    drawWindowSetCursor dw $ Just cs
  cid <- widget `onDestroy` widgetDestroy eb
  eb `on` unrealize $ signalDisconnect cid
  widgetAddEvents eb [ButtonPressMask, ButtonReleaseMask,
                      Button1MotionMask, PointerMotionHintMask]
  eb `set` [ widgetCanFocus := True ]
  eb `on` buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    (x, _)     <- eventCoordinates
    liftIO $ do
      widgetGrabFocus eb
      top <- widgetGetToplevel eb
      Just (wr, _) <- widgetTranslateCoordinates eb widget 0 0
      Just (tx, _) <- widgetTranslateCoordinates eb top (round x) 0
      cid <- eb `on` motionNotifyEvent $ do
        (x', _) <- eventCoordinates
        liftIO $ do
          Just (tx', _) <- widgetTranslateCoordinates eb top (round x') 0
          widget `set` [ widgetWidthRequest := max 50 $ wr + tx' - tx ]
        return True
      mfix $ \cid2 -> do
        eb `on` buttonReleaseEvent $ liftIO $ do
          signalDisconnect cid
          signalDisconnect cid2
          return True
      return ()

instance CompoundWidget ScrollBox where
  type Outer ScrollBox = Viewport
  outer = sViewport
