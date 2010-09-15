-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Mar. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.View
  ( PropertyView
  , makePropertyView
  , propertyViewStore
  , propertyViewRight
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef
import Data.List
import Data.Char
import Data.Maybe

import Graphics.UI.Gtk

import Atoms
import Compound
import Editor
import Utils
import Properties.Property
import Properties.Model


data PropertyView a
  = PropertyView
    { vPaned            :: HPaned
    , vLeft             :: TreeView
    , propertyViewStore :: ListStore (Property, a)
    , propertyViewRight :: TreeView
    , vModified         :: IORef Bool
    }

instance CompoundWidget (PropertyView a) where
  type Outer (PropertyView a) = HPaned
  outer = vPaned

instance EditorWidget (PropertyView a) where
  type Data (PropertyView a) = [(Property, a)]
  getData       = propertyViewGetData
  setData       = propertyViewSetData
  clearData     = propertyViewClearData
  setupView     = propertyViewSetupView
  focusView     = propertyViewFocusView
  getState      = propertyViewGetState
  resetModified = propertyViewResetModified

propertyViewGetData =
  listStoreToList . propertyViewStore

propertyViewSetData v d = do
  listStoreClear $ propertyViewStore v
  mapM_ (listStoreAppend $ propertyViewStore v) d

propertyViewClearData =
  listStoreClear . propertyViewStore

propertyViewSetupView pm =
  treeViewSetCursor (vLeft pm) [0] Nothing

propertyViewFocusView =
  widgetGrabFocus . vLeft

propertyViewGetState =
  liftM (True, ) . readIORef . vModified

propertyViewResetModified =
  flip writeIORef False . vModified

makePropertyView make _ notify = do
  modified <- newIORef False
  let onChanged = do
        writeIORef modified True
        notify

  paned <- hPanedNew
  containerSetBorderWidth paned 7

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  panedPack1 paned scroll True False

  left <- treeViewNewWithModel propertyStore
  treeViewSetHeadersVisible left False

  sel <- treeViewGetSelection left
  treeSelectionSetMode sel SelectionMultiple

  column <- treeViewColumnNew
  treeViewAppendColumn left column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell propertyStore $ \prop ->
    [ cellText := propName prop ]

  containerAdd scroll left

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  panedPack2 paned scroll True False

  store <- listStoreNew []
  store `on` rowChanged $ const $ const onChanged
  store `on` rowInserted $ const $ const onChanged
  store `on` rowDeleted $ const onChanged

  right <- treeViewNewWithModel store
  treeViewSetHeadersVisible right False
  treeViewSetReorderable right True

  sel <- treeViewGetSelection right
  treeSelectionSetMode sel SelectionMultiple

  column <- treeViewColumnNew
  treeViewAppendColumn right column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \(prop, _) ->
    [ cellText := propName prop ]

  containerAdd scroll right

  left `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Return" <- eventKeyName
    liftIO $ do
      sel  <- treeViewGetSelection left
      rows <- treeSelectionGetSelectedRows sel
      forM_ rows $ \[n] ->
        listStoreAppend store . make =<<
        listStoreGetValue propertyStore n

  setupLeftDnD left

  right `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Delete" <- eventKeyName
    liftIO $ do
      sel  <- treeViewGetSelection right
      rows <- treeSelectionGetSelectedRows sel
      mapM_ (listStoreRemove store . head) $ reverse rows

  setupRightDnD store right make

  return PropertyView { vPaned            = paned
                      , vLeft             = left
                      , propertyViewStore = store
                      , propertyViewRight = right
                      , vModified         = modified
                      }


setupLeftDnD left = do
  targetList <- targetListNew
  targetListAdd targetList propertyNameList [TargetSameApp] 0

  dragSourceSet left [Button1] [ActionCopy]
  dragSourceSetTargetList left targetList

  sel <- treeViewGetSelection left
  left `on` dragDataGet $ \_ _ _ -> do
    names <- liftIO $ do
      rows <- treeSelectionGetSelectedRows sel
      forM rows $ liftM propName . listStoreGetValue propertyStore . head
    selectionDataSetStringList names
    return ()

  return ()


setupRightDnD store view make = do
  targetList <- targetListNew
  targetListAdd targetList indexList [TargetSameWidget] 0

  dragSourceSet view [Button1] [ActionMove]
  dragSourceSetTargetList view targetList

  sel <- treeViewGetSelection view
  view `on` dragDataGet $ \_ _ _ -> do
    rows <- liftIO $ treeSelectionGetSelectedRows sel
    selectionDataSet selectionTypeInteger $ map head rows
    return ()

  targetList <- targetListNew
  targetListAdd targetList indexList [TargetSameWidget] 0
  targetListAdd targetList propertyNameList [TargetSameApp] 1

  dragDestSet view [DestDefaultMotion, DestDefaultHighlight]
    [ActionCopy, ActionMove]
  dragDestSetTargetList view targetList

  dropRef <- newIORef False

  view `on` dragDrop $ \ctxt _ tstamp -> do
    maybeTarget <- dragDestFindTarget view ctxt (Just targetList)
    case maybeTarget of
      Just target -> do
        writeIORef dropRef True
        dragGetData view ctxt target tstamp
        return True
      Nothing ->
        return False

  view `on` dragDataReceived $ \ctxt (_, y) infoId tstamp -> do
    drop <- liftIO $ readIORef dropRef
    when drop $ do
      liftIO $ writeIORef dropRef False
      case infoId of
        0 -> do
          rows <- selectionDataGet selectionTypeInteger
          fmaybeM_ rows $ \rows -> liftIO $ do
            base <- getTargetRow store view y pred
            forM_ (reorder base rows) $ \(f, t) -> do
              v <- listStoreGetValue store f
              listStoreRemove store f
              listStoreInsert store t v
        1 -> do
          names <- selectionDataGetStringList
          liftIO $ do
            props <- map make . catMaybes <$> mapM property names
            base  <- getTargetRow store view y id
            zipWithM_ (listStoreInsert store) [base .. ] props
      liftIO $ dragFinish ctxt True True tstamp

  view `on` dragDataDelete $ \_ ->
    treeSelectionUnselectAll sel

getTargetRow store view y f = do
  maybePos <- treeViewGetPathAtPos view (0, y)
  case maybePos of
    Just ([n], _, _) ->
      return n
    Nothing ->
      f <$> listStoreGetSize store

reorder = reorderDown 0
  where reorderDown _ _ [] = []
        reorderDown dec base rows@(r:rs)
          | r <= base = (r - dec, base) : reorderDown (dec + 1) base rs
          | otherwise = reorderUp (if dec /= 0 then base + 1 else base) rows
        reorderUp _ [] = []
        reorderUp base (r:rs)
          | r == base = reorderUp (base + 1) rs
          | otherwise = (r, base) : reorderUp (base + 1) rs

selectionDataSetStringList =
  selectionDataSet selectionTypeInteger . intercalate [0] . map (map ord)

selectionDataGetStringList =
  maybe [] brk <$> selectionDataGet selectionTypeInteger
  where brk text = case break (== 0) text of
          (name, [])       -> [map chr name]
          (name, _ : rest) -> (map chr name) : brk rest

