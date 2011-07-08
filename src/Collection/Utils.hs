-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

{-# LANGUAGE GADTs, UndecidableInstances #-}

module Collection.Utils
  ( selectAll
  , invertSelection
  , setupViewFocus
  , addToPlaylist
  , saveCollection
  , renameCollection
  , deleteCollections
  , CollBuilder (..)
  , onCollBuilt
  , ViewItem (..)
  , VI (..)
  , Killable (..)
  , killThis
  , killNext
  , setNext
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (selectAll)

import XMMS2.Client

import XMMS
import Utils
import UI
import Compound

import Collection.Common


selectAll =
  treeSelectionSelectAll

invertSelection sel = do
  rows <- treeSelectionGetSelectedRows sel
  treeSelectionSelectAll sel
  mapM_ (treeSelectionUnselectPath sel) rows

setupViewFocus abRef view aef ab = do
  view `on` focusInEvent $ liftIO $ do
    writeIORef abRef ab
    aef
    return False

addToPlaylist replace coll = do
  when replace $ playlistClear xmms Nothing >> return ()
  playlistAddCollection xmms Nothing coll []
  return ()

saveCollection coll = do
  res  <- runDlg "Save collection" False (const True) ""
  withJust res $ \name -> do
    collSave xmms coll name "Collections"
    return ()

renameCollection [old] = do
  res <- runDlg "Rename collection" False (/= old) old
  withJust res $ \new -> do
    collRename xmms old new "Collections"
    return ()
renameCollection _ = return ()

deleteCollections =
  mapM_ (\name -> collRemove xmms name "Collections")

runDlg title enable isOk init = do
  dialog <- dialogNew
  windowSetTitle dialog title
  windowSetTransientFor dialog window
  windowSetModal dialog True
  windowGroupAddWindow windowGroup dialog

  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk
  dialogSetDefaultResponse dialog ResponseOk
  dialogSetResponseSensitive dialog ResponseOk enable

  box <- vBoxNew False 0
  containerSetBorderWidth box 7
  upper <- dialogGetUpper dialog
  containerAdd upper box

  entry <- entryNew
  entrySetText entry init
  editableSelectRegion entry 0 (-1)
  editableSetPosition entry (-1)
  boxPackStart box entry PackNatural 0

  let ok = do
        new <- trim <$> entryGetText entry
        return $ not (null new) && isOk new
      check = dialogSetResponseSensitive dialog ResponseOk =<< ok
      checkInsert str pos = check >> return (length str + pos)
      checkDelete _ _     = check
  entry `onEntryActivate` do
    ok <- ok
    when ok $ dialogResponse dialog ResponseOk
  entry `afterInsertText` checkInsert
  entry `afterDeleteText` checkDelete

  widgetShowAll dialog
  resp <- dialogRun dialog
  new  <- trim <$> entryGetText entry
  widgetDestroy dialog

  return $ case resp of
    ResponseOk -> Just new
    _          -> Nothing


class CollBuilder b where
  withBuiltColl :: b -> (Coll -> IO ()) -> IO ()
  treeViewSel   :: b -> (TreeView, TreeSelection)

onCollBuilt env b f = do
  let (view, sel) = treeViewSel b
      doit = withBuiltColl b $ \c -> do
        killNext b
        n <- f c
        setNext b n
        addView env n
  view `on` keyPressEvent $ tryEvent $ do
    "Return" <- eventKeyName
    []       <- eventModifier
    liftIO doit
  view `on` buttonPressEvent $ tryEvent $ do
    LeftButton  <- eventButton
    DoubleClick <- eventClick
    (x, y)      <- eventCoordinates
    liftIO $ do
      Just (p, _, _) <- treeViewGetPathAtPos view (round x, round y)
      treeSelectionSelectPath sel p
      doit

class ViewItem i where
  nextVIRef :: i -> IORef VI

data VI where
  VI   :: (ViewItem i, Killable i) => i -> VI
  None :: VI

class Killable k where
  kill :: k -> IO ()

instance (CompoundWidget w, WidgetClass (Outer w)) => Killable w where
  kill = widgetDestroy . outer

killThis :: (ViewItem i, Killable i) => i -> IO ()
killThis vi = do
  killNext vi
  kill vi

killNext :: ViewItem i => i -> IO ()
killNext vi = do
  next <- readIORef $ nextVIRef vi
  case next of
    VI ni -> do
      killThis ni
      writeIORef (nextVIRef vi) None
    _     -> return ()

setNext :: (ViewItem t, ViewItem n, Killable n) => t -> n -> IO ()
setNext t n =
  writeIORef (nextVIRef t) $ VI n
