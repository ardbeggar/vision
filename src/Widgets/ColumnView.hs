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

module Widgets.ColumnView
  ( ColumnView
  , columnViewNew
  , columnViewAdd
  ) where

import Control.Monad.Fix
import Control.Monad.Trans

import Graphics.UI.Gtk

import Compound


data ColumnView
  = ColumnView { _box      :: HBox
               , _viewport :: Viewport
               }

columnViewNew = do
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
  return ColumnView { _box      = box
                    , _viewport = viewport
                    }

columnViewAdd cv widget = do
  handle <- eventBoxNew
  eventBoxSetVisibleWindow handle True
  widgetSetSizeRequest handle 5 (-1)
  widgetShowAll handle
  setupResize handle widget
  boxPackStart (_box cv) widget PackNatural 0
  boxPackStart (_box cv) handle PackNatural 0

setupResize handle widget = do
  handle `after` realize $ do
    cs <- cursorNew SbHDoubleArrow
    dw <- widgetGetDrawWindow handle
    drawWindowSetCursor dw $ Just cs
  cid <- widget `onDestroy` widgetDestroy handle
  handle `on` unrealize $ signalDisconnect cid
  widgetAddEvents handle [ButtonPressMask, ButtonReleaseMask,
                          Button1MotionMask, PointerMotionHintMask]
  handle `set` [ widgetCanFocus := True ]
  handle `on` buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    (x, _)     <- eventCoordinates
    liftIO $ do
      widgetGrabFocus handle
      top <- widgetGetToplevel handle
      Just (wr, _) <- widgetTranslateCoordinates handle widget 0 0
      Just (tx, _) <- widgetTranslateCoordinates handle top (round x) 0
      cid <- handle `on` motionNotifyEvent $ do
        (x', _) <- eventCoordinates
        liftIO $ do
          Just (tx', _) <- widgetTranslateCoordinates handle top (round x') 0
          widget `set` [ widgetWidthRequest := max 50 $ wr + tx' - tx ]
        return True
      mfix $ \cid2 -> do
        handle `on` buttonReleaseEvent $ liftIO $ do
          signalDisconnect cid
          signalDisconnect cid2
          return True
      return ()

instance CompoundWidget ColumnView where
  type Outer ColumnView = Viewport
  outer = _viewport
