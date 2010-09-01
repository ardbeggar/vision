-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Sep. 2010
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

{-# LANGUAGE DoRec #-}

module Editor
  ( EditorWidget (..)
  , EditorDialog
  , makeEditorDialog
  , runEditorDialog
  ) where

import Control.Monad

import Graphics.UI.Gtk

import Compound
import Utils


class CompoundWidget w => EditorWidget w where
  type Data w
  setData   :: w -> Data w -> IO ()
  getData   :: w -> IO (Data w)
  setupView :: w -> IO ()


data (WindowClass p, EditorWidget e) => EditorDialog p e
  = EditorDialog
    { eParent      :: p
    , eDialog      :: Dialog
    , eEditor      :: e
    }

instance (WindowClass p, EditorWidget e) => CompoundWidget (EditorDialog p e) where
  type Outer (EditorDialog p e) = Dialog
  outer = eDialog


makeEditorDialog parent makeEditor = do
  dialog <- dialogNew

  hideOnDeleteEvent dialog
  parent `onDestroy` do widgetDestroy dialog

  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk

  editor <- makeEditor $ \modified valid -> do
    dialogSetResponseSensitive dialog ResponseOk valid
    dialogSetResponseSensitive dialog ResponseApply modified

  upper <- dialogGetUpper dialog
  boxPackStartDefaults upper $ outer editor
  widgetShowAll $ outer editor

  return EditorDialog { eParent      = parent
                      , eDialog      = dialog
                      , eEditor      = editor
                      }

runEditorDialog e get set modal = do
  let parent      = eParent e
      dialog      = eDialog e
      editor      = eEditor e

  windowSetTransientFor dialog parent
  windowSetModal dialog modal

  when modal $ do
    windowGroup <- windowGroupNew
    windowGroupAddWindow windowGroup parent
    windowGroupAddWindow windowGroup dialog

  setData editor =<< get
  setupView editor
  rec { cid <- dialog `onResponse` \resp -> do
           signalDisconnect cid
           widgetHide dialog
           when (resp == ResponseOk) $ set =<< getData editor
      }
  windowPresent dialog
