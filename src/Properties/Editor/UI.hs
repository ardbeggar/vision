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

import Graphics.UI.Gtk

import Context
import Utils


data UI
  = UI { uLock   :: MVar ()
       , uDialog :: Dialog
       }

dialog = uDialog context

tryLock f = maybe (return ()) (const f) =<< tryTakeMVar (uLock context)
unlock    = putMVar (uLock context) ()



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

  dialog `onResponse` \resp ->
    unless (resp == ResponseApply) $ do
      widgetHide dialog
      unlock

  return ?context

showPropertyEditor _ = do
  tryLock $ do
    putStrLn "Locked!"
  widgetHide dialog
  windowPresent dialog

initContext = do
  lock   <- newMVar ()
  dialog <- dialogNew
  return $ augmentContext
    UI { uLock   = lock
       , uDialog = dialog
       }
