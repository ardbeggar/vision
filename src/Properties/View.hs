-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Mar. 2010
--
--  Copyright (C) 2009-2011 Oleg Belozeorov
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

propertyViewGetData :: PropertyView a -> IO [(Property, a)]
propertyViewGetData = listStoreToList . propertyViewStore

propertyViewSetData :: PropertyView a -> [(Property, a)] -> IO ()
propertyViewSetData v d = do
  listStoreClear $ propertyViewStore v
  mapM_ (listStoreAppend $ propertyViewStore v) d
  writeIORef (vSelected v) $ Set.fromList $ map (propName . fst) d
  treeModelFilterRefilter (vFilter v)

propertyViewClearData :: PropertyView a -> IO ()
propertyViewClearData = flip propertyViewSetData []

propertyViewSetupView :: PropertyView a -> IO ()
propertyViewSetupView pm = treeViewSetCursor (vLeft pm) [0] Nothing

propertyViewFocusView :: PropertyView a -> IO ()
propertyViewFocusView = widgetGrabFocus . vLeft

propertyViewGetState :: PropertyView a -> IO (Bool, Bool)
propertyViewGetState = liftM (True, ) . readIORef . vModified

propertyViewResetModified :: PropertyView a -> IO ()
propertyViewResetModified = flip writeIORef False . vModified

makePropertyView :: WithModel
  => (Property -> (Property, a))
  -> t
  -> IO ()
  -> IO (PropertyView a)
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
  treeModelFilterSetVisibleFunc filter $ \iter -> do
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
      takeProps = do
        sel    <- treeViewGetSelection left
        rows   <- treeSelectionGetSelectedRows sel
        rows   <- mapM (liftM head . treeModelFilterConvertPathToChildPath filter) rows
        props  <- mapM (listStoreGetValue propertyStore) rows
        cursor <- treeViewGetCursor left
        updFilter Set.union props
        case cursor of
          ([n], c) -> do
            size1 <- listStoreGetSize propertyStore
            size2 <- Set.size <$> readIORef selected
            let up = length $ takeWhile (n >) rows
                n' = min (n - up) $ max 0 (size1 - size2 - 1)
                c' = maybe Nothing (Just . (, False)) c
            treeViewSetCursor left [n'] c'
          _ -> return ()
        return props
      addProps =
        mapM_ (listStoreAppend store . make) =<< takeProps

  left `onRowActivated` \_ _ ->
    addProps

  left `on` keyPressEvent $ tryEvent $ do
    []       <- eventModifier
    "Return" <- eventKeyName
    liftIO addProps

  setupLeftDnD left takeProps

  right `onRowActivated` \_ _ ->
    deleteSelectedRows store right updFilter

  right `on` keyPressEvent $ do
    m <- eventModifier
    k <- eventKeyName
    case (m, k) of
      ([], "Delete") -> do
        liftIO $ deleteSelectedRows store right updFilter
        return True
      (_, "Return")  -> return True
      _              -> return False

  setupRightDnD store right make updFilter

  return PropertyView { vPaned            = paned
                      , vFilter           = toTreeModelFilter filter
                      , vLeft             = left
                      , propertyViewStore = store
                      , propertyViewRight = right
                      , vModified         = modified
                      , vSelected         = selected
                      }

setupLeftDnD :: TreeView -> IO [Property] -> IO ()
setupLeftDnD left takeProps = do
  targetList <- targetListNew
  targetListAdd targetList propertyNameListTarget [TargetSameApp] 0

  dragSourceSet left [Button1] [ActionCopy]
  dragSourceSetTargetList left targetList

  left `on` dragDataGet $ \_ _ _ ->
    selectionDataSetStringList =<<
    liftIO (map propName <$> takeProps)

  setupDragDest left
    [DestDefaultMotion, DestDefaultHighlight]
    [ActionMove]
    [ indexListTarget :>: \_ _ -> do
         liftIO $ signalStopEmission left "drag_data_received"
         return (True, True)
    ]

  return ()

setupRightDnD :: WithModel
  => ListStore (Property, a)
  -> TreeView
  -> (Property -> (Property, a))
  -> ((Set String -> Set String -> Set String) -> [Property] -> IO ())
  -> IO ()
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

  view `on` dragDataDelete $ const $
    deleteSelectedRows store view updFilter

  return ()

deleteSelectedRows ::
     ListStore (Property, a)
  -> TreeView
  -> ((Set String -> Set String -> Set String) -> [Property] -> IO ())
  -> IO ()
deleteSelectedRows store view updFilter = do
  sel    <- treeViewGetSelection view
  rows   <- map head <$> treeSelectionGetSelectedRows sel
  props  <- mapM (listStoreGetValue store) rows
  cursor <- treeViewGetCursor view
  mapM_ (listStoreRemove store) $ reverse rows
  updFilter (flip Set.difference) $ map fst props
  case cursor of
    ([n], c) -> do
      size <- listStoreGetSize store
      let up = length $ takeWhile (n >) rows
          n' = min (n - up) $ max 0 (size - 1)
          c' = maybe Nothing (Just . (, False)) c
      treeViewSetCursor view [n'] c'
    _ -> return ()
