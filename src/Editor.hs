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

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import Data.Maybe

import Graphics.UI.Gtk

import Compound
import Utils


class CompoundWidget w => EditorWidget w where
  type Data w
  setData       :: w -> Data w -> IO ()
  getData       :: w -> IO (Data w)
  clearData     :: w -> IO ()
  setupView     :: w -> IO ()
  focusView     :: w -> IO ()
  getState      :: w -> IO (Bool, Bool)
  resetModified :: w -> IO ()

  clearData = const $ return ()
  setupView = const $ return ()
  focusView = const $ return ()


data EditorWidget e => EditorDialog e
  = EditorDialog
    { eLock   :: MVar ()
    , eDialog :: Dialog
    , eEditor :: e
    }

instance EditorWidget e => CompoundWidget (EditorDialog e) where
  type Outer (EditorDialog e) = Dialog
  outer = eDialog


makeEditorDialog buttons makeEditor setup = do
  lock <- newMVar ()

  dialog <- dialogNew
  hideOnDeleteEvent dialog
  dialogSetHasSeparator dialog False

  mapM_ (uncurry $ dialogAddButton dialog) buttons
  dialogAddButton dialog stockCancel ResponseCancel
  dialogAddButtonCR dialog stockOk ResponseOk

  rec { editor <- makeEditor dialog $ updateState dialog editor }

  upper <- dialogGetUpper dialog
  boxPackStartDefaults upper $ outer editor
  widgetShowAll $ outer editor

  let e = EditorDialog { eLock   = lock
                       , eDialog = dialog
                       , eEditor = editor
                       }
  setup e
  return e

runEditorDialog e get set modal parent = do
  let dialog = eDialog e
      editor = eEditor e
      lock   = eLock e

  locked <- isJust <$> tryTakeMVar lock
  when locked $ do
    windowSetModal dialog modal
    windowSetTransientFor dialog parent
    when modal $ do
      windowGroup <- windowGroupNew
      windowGroupAddWindow windowGroup parent
      windowGroupAddWindow windowGroup dialog

    setData editor =<< get
    resetModified editor
    updateState dialog editor
    dialogSetDefaultResponse dialog ResponseOk
    setupView editor
    focusView editor
    rec { cid <- dialog `onResponse` \resp -> do
             let done = do
                   signalDisconnect cid
                   widgetHide dialog
                   clearData editor
                   putMVar lock ()
             dialogSetDefaultResponse dialog ResponseOk
             focusView editor
             case resp of
               ResponseApply -> do
                 (valid, modified) <- getState editor
                 when (valid && modified) (set =<< getData editor)
                 resetModified editor
                 updateState dialog editor
               ResponseOk -> do
                 (valid, modified) <- getState editor
                 when valid $ do
                   when modified (set =<< getData editor)
                   done
               _ -> done
        }
    return ()

  windowPresent dialog


updateState dialog editor = postGUIAsync $ do
  (valid, modified) <- getState editor
  dialogSetResponseSensitive dialog ResponseOk valid
  dialogSetResponseSensitive dialog ResponseApply modified
