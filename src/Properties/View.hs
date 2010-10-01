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
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Graphics.UI.Gtk

import Atoms
import Compound
import Editor
import DnD

import Properties.Property
import Properties.Model


data PropertyView a
  = PropertyView
    { vPaned            :: HPaned
    , vFilter           :: TreeModelFilter
    , vLeft             :: TreeView
    , propertyViewStore :: ListStore (Property, a)
    , propertyViewRight :: TreeView
    , vModified         :: IORef Bool
    , vSelected         :: IORef (Set String)
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
  writeIORef (vSelected v) $ Set.fromList $ map (propName . fst) d
  treeModelFilterRefilter (vFilter v)

propertyViewClearData =
  flip propertyViewSetData []

propertyViewSetupView pm =
  treeViewSetCursor (vLeft pm) [0] Nothing

propertyViewFocusView =
  widgetGrabFocus . vLeft

propertyViewGetState =
  liftM (True, ) . readIORef . vModified

propertyViewResetModified =
  flip writeIORef False . vModified

makePropertyView make _ notify = do
  selected <- newIORef Set.empty

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

  filter <- treeModelFilterNew propertyStore []
  treeModelFilterSetVisibleFunc filter $ Just $ \iter -> do
    [n]   <- treeModelGetPath propertyStore iter
    prop  <- listStoreGetValue propertyStore n
    seld  <- readIORef selected
    return $ Set.notMember (propName prop) seld

  left <- treeViewNewWithModel filter
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

  let updFilter func props = do
        modifyIORef selected $ func (Set.fromList $ map propName props)
        treeModelFilterRefilter filter
      addProps = do
        sel   <- treeViewGetSelection left
        rows  <- treeSelectionGetSelectedRows sel
        rows  <- mapM (treeModelFilterConvertPathToChildPath filter) rows
        props <- mapM (listStoreGetValue propertyStore . head) rows
        mapM (listStoreAppend store . make) props
        updFilter Set.union props

  left `onRowActivated` \_ _ -> addProps

  left `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Return" <- eventKeyName
    liftIO addProps

  setupLeftDnD filter left

  right `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Delete" <- eventKeyName
    liftIO $ do
      sel   <- treeViewGetSelection right
      rows  <- treeSelectionGetSelectedRows sel
      props <- mapM (listStoreGetValue store . head) rows
      mapM_ (listStoreRemove store . head) $ reverse rows
      updFilter (flip Set.difference) $ map fst props


  setupRightDnD store right make updFilter

  return PropertyView { vPaned            = paned
                      , vFilter           = toTreeModelFilter filter
                      , vLeft             = left
                      , propertyViewStore = store
                      , propertyViewRight = right
                      , vModified         = modified
                      , vSelected         = selected
                      }


setupLeftDnD filter left = do
  targetList <- targetListNew
  targetListAdd targetList propertyNameListTarget [TargetSameApp] 0

  dragSourceSet left [Button1] [ActionCopy]
  dragSourceSetTargetList left targetList

  sel <- treeViewGetSelection left
  left `on` dragDataGet $ \_ _ _ -> do
    names <- liftIO $ do
      rows <- treeSelectionGetSelectedRows sel
      rows <- mapM (treeModelFilterConvertPathToChildPath filter) rows
      mapM (liftM propName . listStoreGetValue propertyStore . head) rows
    selectionDataSetStringList names

  setupDragDest left
    [DestDefaultMotion, DestDefaultHighlight]
    [ActionMove]
    [ indexListTarget :>: \_ _ -> do
         liftIO $ signalStopEmission left "drag_data_received"
         return (True, True)
    ]

  return ()


setupRightDnD store view make updFilter = do
  targetList <- targetListNew
  targetListAdd targetList indexListTarget [TargetSameApp] 0

  dragSourceSet view [Button1] [ActionDefault, ActionMove]
  dragSourceSetTargetList view targetList

  sel <- treeViewGetSelection view
  view `on` dragDataGet $ \_ _ _ -> do
    rows <- liftIO $ treeSelectionGetSelectedRows sel
    selectionDataSet selectionTypeInteger $ map head rows
    return ()

  setupDragDest view
    [DestDefaultMotion, DestDefaultHighlight]
    [ActionCopy, ActionDefault]
    [ indexListTarget :>: reorderRows store view
      (mapM_ $ \(f, t) -> do
          v <- listStoreGetValue store f
          listStoreRemove store f
          listStoreInsert store t v)
    , propertyNameListTarget :>: \_ (_, y) -> do
         names <- selectionDataGetStringList
         liftIO $ do
           props <- map make . catMaybes <$> mapM property names
           base  <- getTargetRow store view y False
           zipWithM_ (listStoreInsert store) [base .. ] props
           updFilter Set.union $ map fst props
         return (True, False)
    ]

  view `on` dragDataDelete $ \_ -> do
    rows  <- treeSelectionGetSelectedRows sel
    props <- mapM (listStoreGetValue store . head) rows
    mapM_ (listStoreRemove store . head) $ reverse rows
    updFilter (flip Set.difference) $ map fst props
