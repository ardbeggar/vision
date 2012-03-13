-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 10 Mar. 2010
--
--  Copyright (C) 2009-2011 Oleg Belozeorov
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Collection.List.View
  ( ListView (..)
  , mkListView
  ) where

import Prelude hiding (mapM_)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Applicative
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.Trans

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.IORef
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

import Graphics.UI.Gtk hiding (selectAll)

import XMMS2.Client

import XMMS
import Utils
import Compound
import Clipboard
import Properties

import Collection.Common
import Collection.Utils
import Collection.List.Model
import Collection.Actions


data ListView
  = V { vView    :: TreeView
      , vSel     :: TreeSelection
      , vNextRef :: IORef VI
      , vStore   :: ListStore (Maybe (String, Coll))
      , vIndex   :: IORef (Map (Maybe String) TreeRowReference)
      , vSelSet  :: IORef (Set (Maybe String))
      , vScroll  :: ScrolledWindow
      }

mkListView = withModel $ do
  let popup = coms eVPopup

  selSet <- newIORef Set.empty

  view <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False
  treeViewSetRulesHint view True

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionMultiple
  setupTreeViewPopup view popup

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowIn
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  containerAdd scroll view

  column <- treeViewColumnNew
  treeViewAppendColumn view column

  cell <- cellRendererToggleNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributes column cell store $ \n ->
    [ cellToggleActive :=>
        Set.member (maybe Nothing (Just . fst) n) <$> readIORef selSet
    ]

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \n ->
    case n of
      Nothing ->
        [ cellText := "All Media", cellTextWeight := 800 ]
      Just cn ->
        [ cellText := fst cn, cellTextWeightSet := False ]

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view . Just $ \str iter ->
    maybe False (isInfixOf (map toLower str) . map toLower . fst) <$>
    (listStoreGetValue store $ listStoreIterToIndex iter)

  nextRef <- newIORef None

  let v = V { vView    = view
            , vSel     = sel
            , vNextRef = nextRef
            , vStore   = store
            , vIndex   = index
            , vSelSet  = selSet
            , vScroll  = scroll
            }

  xcW <- atomically $ newTGWatch connectedV
  tid <- forkIO $ forever $ do
    void $ atomically $ watch xcW
    postGUISync $ do
      killNext v
      writeIORef selSet Set.empty
  view `onDestroy` (killThread tid)

  setupUI v

  widgetShowAll scroll
  return v

instance CollBuilder ListView where
  withBuiltColl lv s f = withColls lv $ withColl lv s f
  treeViewSel lv       = (vView lv, vSel lv)
  withNames lv f       = withColls lv (f . map fst . catMaybes)
  enableActions _      = \ae rows -> do
    aEnableSel ae $ not $ null rows
    aEnableDel ae $ case rows of
      []      -> False
      [0] : _ -> False
      _       -> True
    aEnableRen ae $ case rows of
      [[0]] -> False
      [_]   -> True
      _     -> False

withColls lv f = do
  let store = vStore lv
      sel   = vSel lv
  rows <- treeSelectionGetSelectedRows sel
  unless (null rows) $ do
    colls <- mapM (listStoreGetValue store . head) rows
    f colls

withColl v s f list = do
  when s $ do
    ix <- readIORef $ vIndex v
    ss <- readIORef $ vSelSet v
    let ss' = case list of
          Nothing : _ -> Set.singleton Nothing
          _           -> Set.fromList $ map (liftM fst) list
    writeIORef (vSelSet v) ss'
    forM_ (Set.union ss ss') $ \k ->
      withJust (Map.lookup k ix) $ \r -> do
        p <- treeRowReferenceGetPath r
        Just iter <- treeModelGetIter (vStore v) p
        treeModelRowChanged (vStore v) p iter
  withColl' f list

withColl' _ []            = return ()
withColl' f (Nothing : _) = f =<< fc =<< collUniverse
withColl' f list = do
  uni <- collNew TypeUnion
  mapM_ (collAddOperand uni . snd) $ catMaybes list
  f =<< fc uni

fc coll = do
  flt <- collNew TypeEquals
  collAddOperand flt =<< collUniverse
  collAttributeSet flt "field" "status"
  collAttributeSet flt "value" "1"
  int <- collNew TypeIntersection
  collAddOperand int coll
  collAddOperand int flt
  return int

instance CompoundWidget ListView where
  type Outer ListView = ScrolledWindow
  outer = vScroll

instance FocusChild ListView where
  type Focus ListView = TreeView
  focus = vView

instance ViewItem ListView where
  nextVIRef = vNextRef


setupUI lv = do
  g <- actionGroupNew "view-actions"

  a <- actionNew "add-to-playlist" "_Add to playlist" Nothing (Just stockAdd)
  actionGroupAddActionWithAccel g a (Just "<Control>Return")
  a `on` actionActivated $ withBuiltColl lv False $ addToPlaylist False

  a <- actionNew "replace-playlist" "_Replace playlist" Nothing Nothing
  actionGroupAddActionWithAccel g a (Just "<Control><Shift>Return")
  a `on` actionActivated $ withBuiltColl lv False $ addToPlaylist True

  a <- actionNew "copy" "_Copy" Nothing (Just stockCopy)
  actionGroupAddActionWithAccel g a (Just "<Control>c")
  a `on` actionActivated $ withBuiltColl lv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ copyIds ids

  a <- actionNew "select-all" "_Select all" Nothing (Just stockSelectAll)
  actionGroupAddActionWithAccel g a (Just "<Control>a")
  a `on` actionActivated $ selectAll $ vSel lv

  a <- actionNew "invert-selection" "_Invert selection" Nothing (Just stockSelectAll)
  actionGroupAddActionWithAccel g a (Just "<Control><Shift>a")
  a `on` actionActivated $ invertSelection $ vSel lv

  a <- actionNew "edit-properties" "_Edit properties" Nothing (Just stockEdit)
  actionGroupAddActionWithAccel g a (Just "<Alt>Return")
  a `on` actionActivated $ withBuiltColl lv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyEditor ids

  a <- actionNew "export-properties" "E_xport properties…" Nothing (Just stockSave)
  actionGroupAddActionWithAccel g a (Just "")
  a `on` actionActivated $ withBuiltColl lv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyExport ids

  a <- actionNew "save-collection" "_Save collection…" Nothing (Just stockSave)
  actionGroupAddActionWithAccel g a (Just "<Control>s")
  a `on` actionActivated $ withBuiltColl lv False saveCollection

  a <- actionNew "rename-collection" "Rena_me collection…" Nothing Nothing
  actionGroupAddActionWithAccel g a (Just "")
  a `on` actionActivated $ withNames lv renameCollection

  a <- actionNew "delete-collections" "_Delete collections" Nothing Nothing
  actionGroupAddActionWithAccel g a (Just "")
  a `on` actionActivated $ withNames lv deleteCollections

  let view  = vView lv
  tag <- newUITag

  view `on` focusInEvent $ do
    liftIO $ mergeUI tag g (Just ui)
    return False

  view `onDestroy` (removeUI tag)

  return ()

ui =
  [ ( "ui/view-popup/playlist-actions",
      [ Just "add-to-playlist"
      , Just "replace-playlist"
      ]
    )
  , ( "ui/view-popup/clipboard-actions",
      [ Just "copy" ]
    )
  , ( "ui/view-popup/selection-actions",
      [ Just "select-all"
      , Just "invert-selection"
      ]
    )
  , ( "ui/view-popup/property-actions",
      [ Just "edit-properties" ]
    )
  , ( "ui/view-popup/collection-actions",
      [ Just "save-collection"
      , Just "rename-collection"
      , Just "delete-collections"
      ]
    )
  ]
