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
  ( initUI
  , window
  , contents
  , setWindowTitle
  , getWidget
  , addUIFromFile
  , insertActionGroup
  , addUIActions
  , getAction
  , windowGroup
  , makeUI
  , makeBuilder
  , action
  , actions
  , bindAction
  , bindActions
  ) where

import Control.Applicative

import Data.Maybe

import Graphics.UI.Gtk

import Context
import Environment
import About


data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       , uWindowGroup :: WindowGroup
       }

window        = uWindow context
contents      = uContents context
uiManager     = uManager context
uiActionGroup = uActionGroup context
windowGroup   = uWindowGroup context

setWindowTitle = windowSetTitle window
addUIActions = actionGroupAddActions uiActionGroup
insertActionGroup = uiManagerInsertActionGroup uiManager
addUIFromFile = uiManagerAddUiFromFile uiManager . uiFilePath
maybeGetWidget cast name = fmap cast <$> uiManagerGetWidget uiManager name
getWidget cast name = fromJust <$> maybeGetWidget cast name
getAction group name = fromJust <$> actionGroupGetAction group name


initUI builder = do
  context <- augmentContext <$> makeUI builder
  let ?context = context

  mapM_ (uncurry $ maybeBindAction builder)
    [ ("close", widgetDestroy window)
    , ("quit",  mainQuit)
    , ("about", showAbout window)
    ]

  return context

makeUI builder = do
  window        <- builderGetObject builder castToWindow "main-window"
  contents      <- builderGetObject builder castToVBox "contents"
  uiManager     <- builderGetObject builder castToUIManager "ui-manager"
  uiActionGroup <- builderGetObject builder castToActionGroup "ui-actions"
  windowGroup   <- windowGroupNew
  return UI { uWindow      = window
            , uContents    = contents
            , uManager     = uiManager
            , uActionGroup = uiActionGroup
            , uWindowGroup = windowGroup
            }

makeBuilder name = do
  builder <- builderNew
  builderAddFromFile builder $ gladeFilePath name
  return builder

action builder name =
  builderGetObject builder castToAction name

actions builder =
  mapM (action builder)

bindAction builder name func = do
  a <- action builder name
  a `on` actionActivated $ func

bindActions builder =
  mapM (uncurry $ bindAction builder)

maybeBindAction builder name func = do
  ma <- builderGetObjectRaw builder name
  case ma of
    Just a  -> Just <$> on (castToAction a) actionActivated func
    Nothing -> return Nothing

