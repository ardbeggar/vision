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
import Control.Applicative

import Data.Maybe

import Graphics.UI.Gtk

import Context
import Utils
import Medialib
import Properties.Editor.Model
import Properties.Editor.View


data UI
  = UI { uLock   :: MVar ()
       , uDialog :: Dialog
       , uPrevB  :: Button
       , uNextB  :: Button
       , uPtrkB  :: ToggleButton
       }

dialog = uDialog context

tryLock f = maybe (return ()) (const f) =<< tryTakeMVar (uLock context)
unlock    = putMVar (uLock context) ()

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
    unless (resp == ResponseApply) $ do
      widgetHide dialog
      resetModel
      withSignalBlocked cid $
        toggleButtonSetActive ptrkB True
      unlock

  widgetShowAll box
  return ?context

showPropertyEditor ids = do
  tryLock $ do
    let f (id, Just info) = Just (id, info)
        f _               = Nothing
    list <- mapMaybe f . zip ids <$> mapM getInfo ids
    populateModel list
    dialogSetResponseSensitive dialog ResponseApply False
    updateNavButtons
  widgetHide dialog
  windowPresent dialog

initContext = do
  lock   <- newMVar ()
  dialog <- dialogNew
  prevB  <- buttonNewWithMnemonic "_Previous track"
  nextB  <- buttonNewWithMnemonic "_Next track"
  ptrkB  <- toggleButtonNewWithMnemonic "Per _track"
  return $ augmentContext
    UI { uLock   = lock
       , uDialog = dialog
       , uPrevB  = prevB
       , uNextB  = nextB
       , uPtrkB  = ptrkB
       }

updateNavButtons = do
  (ep, en) <- getNavEnables
  widgetSetSensitive prevB ep
  widgetSetSensitive nextB en
