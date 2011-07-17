-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 20 Jun. 2010
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

{-# LANGUAGE RankNTypes #-}

module UI
  ( withUI
  , window
  , contents
  , getWidget
  , windowGroup
  , setWindowTitle
  , informUser
  ) where

import Control.Applicative
import Control.Monad.Trans

import Data.Maybe

import Graphics.UI.Gtk

import About
import Builder


data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       , uWindowGroup :: WindowGroup
       , uInfoBar     :: InfoBar
       , uInfoText    :: Label
       }

window        = uWindow ?ui
contents      = uContents ?ui
uiManager     = uManager ?ui
windowGroup   = uWindowGroup ?ui
infoBar       = uInfoBar ?ui
infoText      = uInfoText ?ui

setWindowTitle title =
  windowSetTitle window title

maybeGetWidget cast name =
  fmap cast <$> uiManagerGetWidget uiManager name

getWidget cast name =
  fromJust <$> maybeGetWidget cast name


newtype Wrap a = Wrap { unWrap :: (?ui :: UI) => a }

withUI    = withUI' . Wrap
withUI' w = do
  ui <- makeUI
  let ?ui = ui

  mapM_ (uncurry maybeBindAction)
    [ ("close", widgetDestroy window)
    , ("quit",  mainQuit)
    , ("about", showAbout window)
    ]

  window `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Escape" <- eventKeyName
    liftIO $ infoBarEmitResponse infoBar dismiss

  unWrap w


makeUI = do
  window              <- getObject castToWindow "main-window"
  contents            <- getObject castToVBox "contents"
  uiManager           <- getObject castToUIManager "ui-manager"
  uiActionGroup       <- getObject castToActionGroup "ui-actions"
  windowGroup         <- windowGroupNew
  (infoBar, infoText) <- makeInfoBar
  return UI { uWindow      = window
            , uContents    = contents
            , uManager     = uiManager
            , uActionGroup = uiActionGroup
            , uWindowGroup = windowGroup
            , uInfoBar     = infoBar
            , uInfoText    = infoText
            }

informUser t m = do
  infoBar `set` [ infoBarMessageType := t ]
  labelSetMarkup infoText m
  widgetSetNoShowAll infoBar False
  widgetShowAll infoBar

makeInfoBar = do
  infoBar <- getObject castToInfoBar "info-bar"

  widgetSetNoShowAll infoBar True
  infoBarAddButton infoBar "Dismiss" dismiss
  infoBarSetDefaultResponse infoBar dismiss
  infoBar `on` infoBarResponse $ const $ widgetHide infoBar
  infoBar `on` infoBarClose $ widgetHide infoBar

  infoText <- labelNew Nothing
  miscSetAlignment infoText 0.0 0.5
  labelSetUseMarkup infoText True
  boxPackStartDefaults infoBar infoText
  boxReorderChild infoBar infoText 0

  return (infoBar, infoText)

dismiss = 0
