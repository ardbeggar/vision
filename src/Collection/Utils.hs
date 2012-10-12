-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
--
--  Copyright (C) 2011, 2012 Oleg Belozeorov
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
  , handleXMMSException
  , SetColl (..)
  , watchConnectionState
  , addActions
  , defAddToPlaylist
  , defReplacePlaylist
  , defCopy
  , defSelectAll
  , defInvertSelection
  , defEditProperties
  , defExportProperties
  , defSaveCollection
  ) where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO

import Data.IORef

import Graphics.UI.Gtk hiding (selectAll, focus)

import XMMS2.Client

import XMMS
import Utils
import UI
import Registry
import Compound
import Properties
import Clipboard

import Collection.Common

selectAll :: TreeSelection -> IO ()
selectAll = treeSelectionSelectAll

invertSelection :: TreeSelection -> IO ()
invertSelection sel = do
  rows <- treeSelectionGetSelectedRows sel
  treeSelectionSelectAll sel
  mapM_ (treeSelectionUnselectPath sel) rows

addToPlaylist :: WithXMMS => Bool -> Coll -> IO ()
addToPlaylist replace coll = do
  when replace $ playlistClear xmms Nothing >> return ()
  playlistAddCollection xmms Nothing coll []
  return ()

saveCollection :: (WithXMMS, WithUI) => Coll -> IO ()
saveCollection coll = do
  res  <- runDlg "Save collection" False (const True) ""
  withJust res $ \name -> do
    collSave xmms coll name "Collections"
    return ()

renameCollection :: (WithXMMS, WithUI) => [String] -> IO ()
renameCollection [old] = do
  res <- runDlg "Rename collection" False (/= old) old
  withJust res $ \new -> do
    collRename xmms old new "Collections"
    return ()
renameCollection _ = return ()

deleteCollections :: (WithXMMS) => [String] -> IO ()
deleteCollections = mapM_ (\name -> collRemove xmms name "Collections")

runDlg :: WithUI
  => String
  -> Bool
  -> (String -> Bool)
  -> String
  -> IO (Maybe String)
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
  withBuiltColl :: b -> Bool -> (Coll -> IO ()) -> IO ()
  treeViewSel   :: b -> (TreeView, TreeSelection)
  withNames     :: b -> ([String] -> IO ()) -> IO ()
  withNames _ = const $ return ()

onCollBuilt ::
  ( WithCommon
  , WidgetClass (Outer f)
  , WidgetClass (Focus f)
  , CompoundWidget f
  , FocusChild f
  , ViewItem b
  , ViewItem f
  , CollBuilder b )
  => b
  -> (Coll -> IO f)
  -> IO (ConnectId TreeView)
onCollBuilt b f = do
  let (view, sel) = treeViewSel b
      doit = withBuiltColl b True $ \c -> do
        n <- f c
        setNext b n
        addView n
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
setNext t n = do
  killNext t
  writeIORef (nextVIRef t) $ VI n

handleXMMSException :: (WithUI, MonadCatchIO m) => m () -> m ()
handleXMMSException f = f `catch` handler
  where handler (XMMSError "invalid collection structure") =
          return ()
        handler e =
          liftIO $ informUser MessageError $ escapeMarkup $ case e of
            XMMSError e -> e
            _           -> "Unknown error"

class SetColl s where
  setColl :: s -> Coll -> IO ()

watchConnectionState ::
  (WithXMMS, WidgetClass w)
  => w
  -> (Bool -> IO ())
  -> IO (ConnectId w)
watchConnectionState owner func = do
  xcW <- atomically $ newTGWatch connectedV
  tid <- forkIO $ forever $ do
    conn <- atomically $ watch xcW
    func conn
  owner `onDestroy` (killThread tid)

addActions :: ActionGroup -> [ActionEntry] -> IO [Action]
addActions g es = forM es $ \e -> do
  a <- actionNew
       (actionEntryName e)
       (actionEntryLabel e)
       (actionEntryTooltip e)
       (actionEntryStockId e)
  actionGroupAddActionWithAccel g a $ actionEntryAccelerator e
  a `on` actionActivated $ actionEntryCallback e
  return a

defAddToPlaylist :: (WithXMMS, CollBuilder b) => b -> ActionEntry
defAddToPlaylist v =
  ActionEntry
  { actionEntryName        = "add-to-playlist"
  , actionEntryLabel       = "_Add to playlist"
  , actionEntryStockId     = Just stockAdd
  , actionEntryAccelerator = Just "<Control>Return"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False $ addToPlaylist False
  }

defReplacePlaylist :: (WithXMMS, CollBuilder b) => b -> ActionEntry
defReplacePlaylist v =
  ActionEntry
  { actionEntryName        = "replace-playlist"
  , actionEntryLabel       = "_Replace playlist"
  , actionEntryStockId     = Nothing
  , actionEntryAccelerator = Just "<Control><Shift>Return"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False $ addToPlaylist True
  }

defCopy :: (WithClipboard, WithXMMS, CollBuilder b) => b -> ActionEntry
defCopy v =
  ActionEntry
  { actionEntryName        = "copy"
  , actionEntryLabel       = "_Copy"
  , actionEntryStockId     = Just stockCopy
  , actionEntryAccelerator = Just "<Control>c"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False $ \coll ->
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ copyIds ids
  }

defSelectAll :: CollBuilder b => b -> ActionEntry
defSelectAll v =
  ActionEntry
  { actionEntryName        = "select-all"
  , actionEntryLabel       = "_Select all"
  , actionEntryStockId     = Just stockSelectAll
  , actionEntryAccelerator = Just "<Control>a"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = selectAll $ snd $ treeViewSel v
  }

defInvertSelection :: CollBuilder b => b -> ActionEntry
defInvertSelection v =
  ActionEntry
  { actionEntryName        = "invert-selection"
  , actionEntryLabel       = "_Invert selection"
  , actionEntryStockId     = Just stockSelectAll
  , actionEntryAccelerator = Just "<Control><Shift>a"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = invertSelection $ snd $ treeViewSel v
  }

defEditProperties ::
  (WithRegistry, WithXMMS, WithUI, CollBuilder b)
  => b
  -> ActionEntry
defEditProperties v =
  ActionEntry
  { actionEntryName        = "edit-properties"
  , actionEntryLabel       = "_Edit properties"
  , actionEntryStockId     = Just stockEdit
  , actionEntryAccelerator = Just "<Alt>Return"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False $ \coll ->
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyEditor ids
  }

defExportProperties ::
  (WithRegistry, WithXMMS, WithUI, CollBuilder b)
  => b
  -> ActionEntry
defExportProperties v =
  ActionEntry
  { actionEntryName        = "export-properties"
  , actionEntryLabel       = "E_xport properties"
  , actionEntryStockId     = Just stockSave
  , actionEntryAccelerator = Just ""
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False $ \coll ->
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyExport ids
  }

defSaveCollection :: (WithXMMS, WithUI, CollBuilder b) => b -> ActionEntry
defSaveCollection v =
  ActionEntry
  { actionEntryName        = "save-collection"
  , actionEntryLabel       = "_Save collection…"
  , actionEntryStockId     = Just stockSave
  , actionEntryAccelerator = Just "<Control>s"
  , actionEntryTooltip     = Nothing
  , actionEntryCallback    = withBuiltColl v False saveCollection
  }
