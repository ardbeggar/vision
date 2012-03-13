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

{-# LANGUAGE Rank2Types #-}

module UI
  ( WithUI
  , withUI
  , window
  , uiManager
  , contents
  , getWidget
  , windowGroup
  , setWindowTitle
  , informUser
  , withUIGlobal
  ) where

import Control.Applicative
import Control.Monad.Trans

import Control.Concurrent.STM

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.UI.Gtk

import About
import Builder


type WithUI a = (?_UI :: UI) => a

data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       , uWindowGroup :: WindowGroup
       , uInfoBar     :: InfoBar
       , uInfoText    :: Label
       }

window        = uWindow ?_UI
contents      = uContents ?_UI
uiManager     = uManager ?_UI
windowGroup   = uWindowGroup ?_UI
infoBar       = uInfoBar ?_UI
infoText      = uInfoText ?_UI

setWindowTitle title =
  windowSetTitle window title

maybeGetWidget cast name =
  fmap cast <$> uiManagerGetWidget uiManager name

getWidget cast name =
  fromJust <$> maybeGetWidget cast name


newtype Wrap a = Wrap { unWrap :: (?_UI :: UI) => a }

withUI  r   = withUI' r . Wrap
withUI' r w = do
  ui <- makeUI
  let ?_UI = ui

  role <- mkWindowRole r
  windowSetRole window role

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


data UIGlobal
  = UIGlobal
    { _roles :: TVar (Map String Int) }

mkUIGlobal = do
  roles <- newTVarIO Map.empty
  return UIGlobal { _roles = roles }

newtype WrapG a = WrapG { unWrapG :: (?_UI_Global :: UIGlobal) => a }

withUIGlobal    = withUIGlobal' . WrapG
withUIGlobal' w = do
  uiGlobal <- mkUIGlobal
  let ?_UI_Global = uiGlobal
  unWrapG w

mkWindowRole h = do
  n <- atomically $ do
    m <- readTVar $ _roles ?_UI_Global
    let (n, m') = Map.insertLookupWithKey (\_ _ o -> o + 1) h 1 m
    writeTVar (_roles ?_UI_Global) m'
    return $! maybe 1 (+ 1) n
  return $ h ++ "#" ++ show n
