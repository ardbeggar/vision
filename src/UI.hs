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
  , WithUIGlobal
  , withUIGlobal
  , mergeUI
  , removeUI
  , newUITag
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent.STM

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

import Graphics.UI.Gtk

import About
import Builder
import Registry
import Utils


data UI
  = UI { uWindow      :: Window
       , uContents    :: VBox
       , uManager     :: UIManager
       , uActionGroup :: ActionGroup
       , uWindowGroup :: WindowGroup
       , uInfoBar     :: InfoBar
       , uInfoText    :: Label
       , _UITagRef    :: IORef Integer
       , _UIRef       :: IORef (Maybe (Integer, [ActionGroup], Maybe MergeId))
       }

type WithUI = ?_UI :: UI

window :: WithUI => Window
window = uWindow ?_UI

contents :: WithUI => VBox
contents = uContents ?_UI

uiManager :: WithUI => UIManager
uiManager = uManager ?_UI

windowGroup :: WithUI => WindowGroup
windowGroup = uWindowGroup ?_UI

infoBar :: WithUI => InfoBar
infoBar = uInfoBar ?_UI

infoText :: WithUI => Label
infoText = uInfoText ?_UI

uiTagRef :: WithUI => IORef Integer
uiTagRef = _UITagRef ?_UI

uiRef :: WithUI => IORef (Maybe (Integer, [ActionGroup], Maybe MergeId))
uiRef = _UIRef ?_UI

setWindowTitle :: WithUI => String -> IO ()
setWindowTitle title = windowSetTitle window title

maybeGetWidget :: WithUI => (Widget -> a) -> String -> IO (Maybe a)
maybeGetWidget cast name = fmap cast <$> uiManagerGetWidget uiManager name

getWidget :: WithUI => (Widget -> a) -> String -> IO a
getWidget cast name = fromJust <$> maybeGetWidget cast name

withUI ::
  (WithBuilder, WithRegistry, WithUIGlobal)
  => String
  -> (WithUI => IO a)
  -> IO a
withUI role func = do
  ui <- makeUI
  let ?_UI = ui

  windowSetRole window =<< mkWindowRole role

  mapM_ (uncurry maybeBindAction)
    [ ("close", widgetDestroy window)
    , ("quit",  mainQuit)
    , ("about", showAbout window)
    ]

  window `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Escape" <- eventKeyName
    liftIO $ infoBarEmitResponse infoBar dismiss

  func

makeUI :: WithBuilder => IO UI
makeUI = do
  window              <- getObject castToWindow "main-window"
  contents            <- getObject castToVBox "contents"
  uiManager           <- getObject castToUIManager "ui-manager"
  uiActionGroup       <- getObject castToActionGroup "ui-actions"
  windowGroup         <- windowGroupNew
  (infoBar, infoText) <- makeInfoBar
  uiTagRef            <- newIORef 0
  uiRef               <- newIORef Nothing
  return UI { uWindow      = window
            , uContents    = contents
            , uManager     = uiManager
            , uActionGroup = uiActionGroup
            , uWindowGroup = windowGroup
            , uInfoBar     = infoBar
            , uInfoText    = infoText
            , _UITagRef    = uiTagRef
            , _UIRef       = uiRef
            }

informUser :: WithUI => MessageType -> Markup -> IO ()
informUser t m = do
  infoBar `set` [ infoBarMessageType := t ]
  labelSetMarkup infoText m
  widgetSetNoShowAll infoBar False
  widgetShowAll infoBar

makeInfoBar :: WithBuilder => IO (InfoBar, Label)
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

dismiss :: Int
dismiss = 0


data UIGlobal
  = UIGlobal
    { _roles :: TVar (Map String Int) }

type WithUIGlobal = ?_UI_Global :: UIGlobal

mkUIGlobal :: IO UIGlobal
mkUIGlobal = do
  roles <- newTVarIO Map.empty
  return UIGlobal { _roles = roles }

withUIGlobal :: (WithUIGlobal => IO a) -> IO a
withUIGlobal func = do
  uiGlobal <- mkUIGlobal
  let ?_UI_Global = uiGlobal
  func

mkWindowRole :: WithUIGlobal => String -> IO String
mkWindowRole role = do
  n <- atomically $ do
    m <- readTVar $ _roles ?_UI_Global
    let (n, m') = Map.insertLookupWithKey (\_ _ o -> o + 1) role 1 m
    writeTVar (_roles ?_UI_Global) m'
    return $! maybe 1 (+ 1) n
  return $ role ++ "#" ++ show n


class UIDefClass def where
  mergeUIDef :: UIManager -> def -> IO MergeId

instance UIDefClass String where
  mergeUIDef = uiManagerAddUiFromString

instance UIDefClass [(String, [Maybe String])] where
  mergeUIDef uiManager list = do
    mergeId <- uiManagerNewMergeId uiManager
    forM_ list $ \(path, actions) ->
      forM_ actions $ add mergeId path
    return mergeId
    where add mergeId path (Just action) =
            uiManagerAddUi uiManager mergeId path action (Just action) [UiManagerAuto] False
          add mergeId path Nothing =
            uiManagerAddUi uiManager mergeId path "" Nothing [UiManagerAuto] False

mergeUI ::
  (WithUI, UIDefClass def)
  => Integer
  -> [ActionGroup]
  -> Maybe def
  -> IO ()
mergeUI tag ags ui = modifyUI Nothing $ do
  forM_ ags $ \ag ->
    uiManagerInsertActionGroup uiManager ag 0
  mid <- case ui of
    Just def -> Just <$> mergeUIDef uiManager def
    Nothing  -> return Nothing
  return $ Just (tag, ags, mid)

removeUI :: WithUI => Maybe Integer -> IO ()
removeUI tag =
  modifyUI tag (return Nothing)

modifyUI ::
  WithUI
  => Maybe Integer
  -> IO (Maybe (Integer, [ActionGroup], Maybe MergeId))
  -> IO ()
modifyUI tag f = do
  mui <- readIORef uiRef
  withJust mui $ \(tag', ags, mmid) ->
    when (fromMaybe tag' tag == tag') $ do
      mapM_ (uiManagerRemoveActionGroup uiManager) ags
      withJust mmid $ \mid ->
        uiManagerRemoveUi uiManager mid
  writeIORef uiRef =<< f

newUITag :: WithUI => IO Integer
newUITag = do
  tag <- readIORef uiTagRef
  writeIORef uiTagRef $ tag + 1
  return tag
