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
  ) where

import Control.Applicative

import Data.Maybe

import Graphics.UI.Gtk

import Context
import Environment


data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       }

window        = uWindow context
contents      = uContents context
uiManager     = uManager context
uiActionGroup = uActionGroup context

initUI = do
  context <- initContext
  let ?context = context

  containerAdd window contents

  windowAddAccelGroup window =<< uiManagerGetAccelGroup uiManager
  insertActionGroup uiActionGroup 1
  addUIActions uiActions
  addUIFromFile "common"

  menubar <- getWidget castToMenuBar "ui/menubar"
  boxPackStart contents menubar PackNatural 0

  return ?context


initContext = do
  window        <- windowNew
  contents      <- vBoxNew False 0
  uiManager     <- uiManagerNew
  uiActionGroup <- actionGroupNew "ui"
  return $ augmentContext
    UI { uWindow      = window
       , uContents    = contents
       , uManager     = uiManager
       , uActionGroup = uiActionGroup
       }

setWindowTitle = windowSetTitle window
addUIActions = actionGroupAddActions uiActionGroup
insertActionGroup = uiManagerInsertActionGroup uiManager
addUIFromFile = uiManagerAddUiFromFile uiManager . uiFilePath
maybeGetWidget cast name = fmap cast <$> uiManagerGetWidget uiManager name
getWidget cast name = fromJust <$> maybeGetWidget cast name
getAction group name = fromJust <$> actionGroupGetAction group name

uiActions =
  [ ActionEntry
    { actionEntryName        = "menubar"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "toolbar"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "quit"
    , actionEntryLabel       = "_Quit"
    , actionEntryStockId     = Just stockQuit
    , actionEntryAccelerator = Just "<Control>q"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = mainQuit
    }
  , ActionEntry
    { actionEntryName        = "close-window"
    , actionEntryLabel       = "_Close window"
    , actionEntryStockId     = Just stockClose
    , actionEntryAccelerator = Just "<Control>w"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = widgetDestroy window
    }
  ]
