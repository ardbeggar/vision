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

import Data.IORef
import Data.List

import Graphics.UI.Gtk hiding (add, remove)

import Context
import Utils
import Medialib
import Handler
import UI
import XMMS
import Properties.Editor.Model
import Properties.Editor.View


data UI
  = UI { uVisible :: MVar (Maybe Dialog)
       , uDialog  :: Dialog
       , uPrevB   :: Button
       , uNextB   :: Button
       , uPtrkB   :: ToggleButton
       }

visible    = uVisible context
setVisible = modifyMVar_ visible . const . return

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

  toggleButtonSetActive ptrkB True
  cid <- ptrkB `onToggled` (togglePerTrack >> updateNavButtons)
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
    when chok $ dialogSetResponseSensitive dialog ResponseApply True

  dialog `onResponse` \resp ->
    case resp of
      ResponseApply -> do
        dialogSetResponseSensitive dialog ResponseApply False
        dialogSetResponseSensitive dialog ResponseOk False
        writeProperties
        dialogSetResponseSensitive dialog ResponseOk True
        widgetGrabFocus view
      ResponseOk    -> do
        dialogSetResponseSensitive dialog ResponseApply False
        dialogSetResponseSensitive dialog ResponseOk False
        writeProperties
        widgetHide dialog
        dialogSetResponseSensitive dialog ResponseOk True
        resetModel
        withSignalBlocked cid $
          toggleButtonSetActive ptrkB True
        setVisible Nothing
      _             -> do
        widgetHide dialog
        resetModel
        withSignalBlocked cid $
          toggleButtonSetActive ptrkB True
        setVisible Nothing

  widgetShowAll box
  return ?context

showPropertyEditor ids =
  modifyMVar_ visible $ \maybeVis ->
    case maybeVis of
      Just vis -> do
        widgetHide vis
        windowSetTransientFor vis window
        windowPresent vis
        return $ Just vis
      Nothing  -> withMediaInfo ids $ \list -> do
        populateModel list
        dialogSetResponseSensitive dialog ResponseApply False
        updateNavButtons
        resetView
        windowSetTransientFor dialog window
        windowPresent dialog
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

withMediaInfo ids f = do
  let ids' = nub ids
      len  = length ids'
      step = len `div` 100

  dialog <- dialogNew
  windowSetDefaultSize dialog 200 (-1)
  dialogSetHasSeparator dialog False
  windowSetTitle dialog "Retrieving properties"
  windowSetTransientFor dialog window

  dialogAddButton dialog "gtk-cancel" ResponseCancel

  upper <- dialogGetUpper dialog
  box   <- vBoxNew False 0
  containerSetBorderWidth box 7
  boxPackStart upper box PackNatural 0

  pbar <- progressBarNew
  progressBarSetText pbar "Retrieving properties"
  boxPackStart box pbar PackNatural 0

  ref <- newIORef (0, ids', [])
  hid <- onMediaInfo . add $ \(id, _, info) -> do
    (ctr, todo, ready) <- readIORef ref
    case todo of
      (i:is) | i == id -> do
        if null is
          then do
          setVisible =<< (f $ reverse ((id, info) : ready))
          widgetDestroy dialog
          return False
          else do
          let ctr' = ctr + 1
          when (step == 0 || ctr' `mod` step == 0) $
            progressBarSetFraction pbar $
              fromIntegral ctr' / fromIntegral len
          requestInfo $ head is
          writeIORef ref (ctr', is, (id, info) : ready)
          return True
      _ ->
        return True

  dialog `onResponse` \_ -> do
    onMediaInfo $ remove hid
    widgetDestroy dialog
    setVisible Nothing

  did <- onServerConnection . add . once $ \conn ->
    unless conn $ do
      flip timeoutAdd 0 $ do
        onMediaInfo $ remove hid
        widgetDestroy dialog
        setVisible Nothing
        return False
      return ()
  dialog `onDestroy` (onServerConnection $ remove did)

  widgetShowAll dialog
  requestInfo $ head ids

  return $ Just dialog
