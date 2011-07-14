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

module UIEnvIO
  ( runUI
  , uiEnv
  , window
  , contents
  , getWidget
  , windowGroup
{-
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
  , informUser
-}
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.EnvIO

import Data.Maybe

import Graphics.UI.Gtk

import About
import Builder


data Ix = Ix

data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       , uWindowGroup :: WindowGroup
       , uInfoBar     :: InfoBar
       , uInfoText    :: Label
       }

uiEnv :: Extract Ix UI
uiEnv = Extract

window        = envsx Ix uWindow
contents      = envsx Ix uContents
uiManager     = envsx Ix uManager
{-
uiActionGroup = envsx Ix uActionGroup
-}
windowGroup   = envsx Ix uWindowGroup
infoBar       = envsx Ix uInfoBar
--infoText      = envsx Ix uInfoText
{-
setWindowTitle = windowSetTitle window
addUIActions = actionGroupAddActions uiActionGroup
insertActionGroup = uiManagerInsertActionGroup uiManager
addUIFromFile = uiManagerAddUiFromFile uiManager . uiFilePath
getAction group name = fromJust <$> actionGroupGetAction group name
-}
maybeGetWidget cast name = do
  uiManager <- uiManager
  liftIO $ fmap cast <$> uiManagerGetWidget uiManager name

getWidget cast name =
  fromJust <$> maybeGetWidget cast name

runUI f = do
  env <- makeUI
  runIn' (env :*:) $> do
    window <- window
    mapM_ (uncurry maybeBindAction)
      [ ("close", liftIO $ widgetDestroy window)
      , ("quit",  liftIO $ mainQuit)
      , ("about", liftIO $ showAbout window)
      ]
    infoBar <- infoBar
    liftIO $ window `on` keyPressEvent $ tryEvent $ do
      []       <- eventModifier
      "Escape" <- eventKeyName
      liftIO $ infoBarEmitResponse infoBar dismiss

  f

makeUI = do
  window              <- getObject castToWindow "main-window"
  contents            <- getObject castToVBox "contents"
  uiManager           <- getObject castToUIManager "ui-manager"
  uiActionGroup       <- getObject castToActionGroup "ui-actions"
  windowGroup         <- liftIO $ windowGroupNew
  (infoBar, infoText) <- makeInfoBar
  return $ mkEnv Ix
    UI { uWindow      = window
       , uContents    = contents
       , uManager     = uiManager
       , uActionGroup = uiActionGroup
       , uWindowGroup = windowGroup
       , uInfoBar     = infoBar
       , uInfoText    = infoText
       }

{-
informUser t m = do
  infoBar  <- infoBar
  infoText <- infoText
  liftIO $ do
    infoBar `set` [ infoBarMessageType := t ]
    labelSetMarkup infoText m
    widgetSetNoShowAll infoBar False
    widgetShowAll infoBar
-}
makeInfoBar = do
  infoBar <- getObject castToInfoBar "info-bar"
  liftIO $ do
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
