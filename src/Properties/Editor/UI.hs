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

module Properties.Editor.UI
  ( initEditorUI
  , showPropertyEditor
  ) where

import Control.Concurrent.MVar
import Control.Monad

import Graphics.UI.Gtk hiding (add, remove)

import Context
import Utils
import Handler
import UI
import XMMS
import WithMediaInfo
import Properties.Editor.Model
import Properties.Editor.View


data UI
  = UI { uVisible :: MVar (Maybe Dialog)
       , uDialog  :: Dialog
       , uPrevB   :: Button
       , uNextB   :: Button
       , uPtrkB   :: ToggleButton
       }

visible = uVisible context

dialog = uDialog context

prevB = uPrevB context
nextB = uNextB context
ptrkB = uPtrkB context


initEditorUI = do
  context <- initContext
  let ?context = context

  hideOnDeleteEvent dialog
  windowSetModal dialog False
  windowSetTitle dialog "Edit properties"
  windowSetDefaultSize dialog 800 600
  dialogSetHasSeparator dialog False

  dialogAddButton   dialog "gtk-apply"  ResponseApply
  dialogAddButton   dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok"     ResponseOk

  box <- vBoxNew False 0
  containerSetBorderWidth box 7
  upper <- dialogGetUpper dialog
  boxPackStartDefaults upper box

  bbox <- hButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  boxSetSpacing bbox 7
  boxPackStart box bbox PackNatural 7

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
      updateTitle True

  dialog `onResponse` \resp ->
    case resp of
      ResponseApply -> do
        doWriteProperties
        dialogSetResponseSensitive dialog ResponseOk True
        widgetGrabFocus view
      ResponseOk    -> do
        doWriteProperties
        hideEditor
      _             ->
        hideEditor

  onServerConnection . add . ever $ \conn ->
    unless conn $ do
      widgetSetSensitive prevB False
      widgetSetSensitive nextB False
      widgetSetSensitive ptrkB False
      dialogSetResponseSensitive dialog ResponseApply False
      dialogSetResponseSensitive dialog ResponseOk False
      updateTitle False

  setupOnHide visible dialog

  widgetShowAll box
  return ?context

doWriteProperties = do
  dialogSetResponseSensitive dialog ResponseApply False
  dialogSetResponseSensitive dialog ResponseOk False
  writeProperties
  updateTitle False

hideEditor = do
  widgetHide dialog
  resetModel

updateTitle m = do
  c <- connected
  windowSetTitle dialog $ case c of
    False    -> "Edit properties (disconnected)"
    True | m -> "Edit properties (modified)"
    _        -> "Edit properties"

showPropertyEditor =
  showWithMediaInfo visible $ \list -> do
    toggleButtonSetActive ptrkB True
    widgetSetSensitive ptrkB True
    populateModel list
    dialogSetResponseSensitive dialog ResponseApply False
    dialogSetResponseSensitive dialog ResponseOk True
    updateNavButtons
    resetView
    windowSetTransientFor dialog window
    updateTitle False
    return $ Just dialog

initContext = do
  visible <- newMVar Nothing
  dialog  <- dialogNew
  prevB   <- buttonNewWithMnemonic "_Previous track"
  nextB   <- buttonNewWithMnemonic "_Next track"
  ptrkB   <- toggleButtonNewWithMnemonic "Per _track"
  return $ augmentContext
    UI { uVisible = visible
       , uDialog  = dialog
       , uPrevB   = prevB
       , uNextB   = nextB
       , uPtrkB   = ptrkB
       }

updateNavButtons = do
  (ep, en) <- getNavEnables
  widgetSetSensitive prevB ep
  widgetSetSensitive nextB en

