-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.Editor.UI
  ( initUI
  , showPropertyEditor
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Monad

import Data.IORef
import Data.Typeable
import Data.Env

import Graphics.UI.Gtk hiding (add, remove)
import XMMS2.Client (MediaId)

import Registry
import Utils
import UI (window)
import qualified UI
import XMMS
import Medialib
import Properties.Editor.Model
import Properties.Editor.View


data UI
  = UI { _lock    :: MVar ()
       , _dialog  :: Dialog
       , _prevB   :: Button
       , _nextB   :: Button
       , _ptrkB   :: ToggleButton
       , _pBar    :: ProgressBar
       , _cancel  :: IORef (Maybe (IO ()))
       }

type WithUI = ?_Properties_Editor_UI :: UI

data Ix = Ix deriving (Typeable)

data UIE
  = UIE { _showPropertyEditor :: UI.WithUI => [MediaId] -> IO () }
  deriving (Typeable)

showPropertyEditor :: (WithRegistry, UI.WithUI) => [MediaId] -> IO ()
showPropertyEditor ids = do
  Just (Env uie) <- getEnv (Extract :: Extract Ix UIE)
  _showPropertyEditor uie ids

tryLock :: WithUI => IO a -> IO Bool
tryLock f =
  maybe (return False) (const $ f >> return True)
  =<< tryTakeMVar (_lock ?_Properties_Editor_UI)

unlock :: WithUI => IO ()
unlock = tryPutMVar (_lock ?_Properties_Editor_UI) () >> return ()

dialog :: WithUI => Dialog
dialog = _dialog ?_Properties_Editor_UI

prevB :: WithUI => Button
prevB = _prevB ?_Properties_Editor_UI

nextB :: WithUI => Button
nextB = _nextB ?_Properties_Editor_UI

ptrkB :: WithUI => ToggleButton
ptrkB = _ptrkB ?_Properties_Editor_UI

pBar :: WithUI => ProgressBar
pBar = _pBar  ?_Properties_Editor_UI

setRetrievalCancel :: WithUI => IO () -> IO ()
setRetrievalCancel = writeIORef (_cancel ?_Properties_Editor_UI) . Just

cancelRetrieval :: WithUI => IO ()
cancelRetrieval = do
  cancel' <- readIORef (_cancel ?_Properties_Editor_UI)
  withJust cancel' id
  writeIORef (_cancel ?_Properties_Editor_UI) Nothing

initUI :: (WithRegistry, WithXMMS, WithModel, WithView) => IO ()
initUI = withMedialib $ do
  ui <- mkUI
  let ?_Properties_Editor_UI = ui

  hideOnDeleteEvent dialog
  windowSetModal dialog False
  windowSetTitle dialog "Edit properties"
  windowSetDefaultSize dialog 800 600
  dialogSetHasSeparator dialog False

  dialogAddButton   dialog stockApply  ResponseApply
  dialogAddButton   dialog stockCancel ResponseCancel
  dialogAddButtonCR dialog stockOk     ResponseOk

  box <- vBoxNew False 0
  containerSetBorderWidth box 7
  upper <- dialogGetUpper dialog
  boxPackStartDefaults upper box

  hbox <- hBoxNew False 15
  boxPackStart box hbox PackNatural 7

  bbox <- hButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  boxSetSpacing bbox 7
  boxPackStart hbox bbox PackNatural 0

  boxPackStartDefaults hbox pBar

  prevB `onClicked` (prevTrack >> updateNavButtons)
  widgetSetCanFocus prevB False
  containerAdd bbox prevB

  nextB `onClicked` (nextTrack >> updateNavButtons)
  widgetSetCanFocus nextB False
  containerAdd bbox nextB

  ptrkB `onToggled` (togglePerTrack >> updateNavButtons)
  widgetSetCanFocus ptrkB False
  containerAdd bbox ptrkB

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll view
  boxPackStartDefaults box scroll

  onPropertyEdited $ \[n] t -> do
    prop <- listStoreGetValue store n
    chok <- changeProperty n prop t
    when chok $ do
      dialogSetResponseSensitive dialog ResponseApply True
      updateTitle True False

  dialog `onResponse` \resp ->
    case resp of
      ResponseApply -> do
        doWriteProperties
        dialogSetResponseSensitive dialog ResponseOk True
        widgetGrabFocus view
      ResponseOk    -> do
        doWriteProperties
        hideEditor
      _             -> do
        cancelRetrieval
        hideEditor

  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    void $ atomically $ watch xcW
    postGUISync $ do
      cancelRetrieval
      widgetSetSensitive prevB False
      widgetSetSensitive nextB False
      widgetSetSensitive ptrkB False
      dialogSetResponseSensitive dialog ResponseApply False
      dialogSetResponseSensitive dialog ResponseOk False
      progressBarSetFraction pBar 0
      progressBarSetText pBar "Disconnected"
      updateTitle False False
      unlock

  widgetShowAll box
  addEnv Ix UIE { _showPropertyEditor = doShowPropertyEditor }
  return ()

doWriteProperties :: (WithXMMS, WithModel, WithUI) => IO ()
doWriteProperties = do
  dialogSetResponseSensitive dialog ResponseApply False
  dialogSetResponseSensitive dialog ResponseOk False
  writeProperties
  updateTitle False False

hideEditor :: (WithModel, WithUI) => IO ()
hideEditor = do
  widgetHide dialog
  resetModel
  unlock

updateTitle :: (WithXMMS, WithUI) => Bool -> Bool -> IO ()
updateTitle m r = do
  c <- connected
  windowSetTitle dialog $ case c of
    False    -> "Edit properties (disconnected)"
    True | m -> "Edit properties (modified)"
    _    | r -> "Edit properties (retrieving)"
    _        -> "Edit properties"

doShowPropertyEditor :: (WithXMMS, WithMedialib, UI.WithUI, WithModel, WithView, WithUI) => [MediaId] -> IO ()
doShowPropertyEditor ids = do
  retr <- tryLock $ do
    dialogSetResponseSensitive dialog ResponseApply False
    dialogSetResponseSensitive dialog ResponseOk False
    widgetSetSensitive ptrkB False
    updateNavButtons
    widgetShow pBar
    resetView
    setRetrievalCancel =<< do
      progressBarSetFraction pBar 0
      progressBarSetText pBar "Retrieving properties"
      retrieveProperties ids $ \prog ->
        case prog of
          Left frac  ->
            progressBarSetFraction pBar frac
          Right list -> do
            toggleButtonSetActive ptrkB True
            widgetSetSensitive ptrkB True
            populateModel list
            dialogSetResponseSensitive dialog ResponseApply False
            dialogSetResponseSensitive dialog ResponseOk True
            updateNavButtons
            widgetHide pBar
            updateTitle False False
  widgetHide dialog
  windowSetTransientFor dialog window
  updateTitle False retr
  windowPresent dialog

mkUI :: IO UI
mkUI = do
  lock   <- newMVar ()
  dialog <- dialogNew
  prevB  <- buttonNewWithMnemonic "_Previous track"
  nextB  <- buttonNewWithMnemonic "_Next track"
  ptrkB  <- toggleButtonNewWithMnemonic "Per _track"
  pBar   <- progressBarNew
  cancel <- newIORef Nothing
  return UI { _lock   = lock
            , _dialog = dialog
            , _prevB  = prevB
            , _nextB  = nextB
            , _ptrkB  = ptrkB
            , _pBar   = pBar
            , _cancel = cancel
            }

updateNavButtons :: (WithModel, WithUI) => IO ()
updateNavButtons = do
  (ep, en) <- getNavEnables
  widgetSetSensitive prevB ep
  widgetSetSensitive nextB en

