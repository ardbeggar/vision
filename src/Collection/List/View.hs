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

import Utils
import Compound

import Collection.Common
import Collection.Utils
import Collection.List.Model


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

  g <- setupUI v
  watchConnectionState view $ \conn -> postGUISync $ do
    killNext v
    writeIORef selSet Set.empty
    actionGroupSetSensitive g conn

  widgetShowAll scroll
  return v

instance CollBuilder ListView where
  withBuiltColl lv s f = withColls lv $ withColl lv s f
  treeViewSel lv       = (vView lv, vSel lv)
  withNames lv f       = withColls lv (f . map fst . catMaybes)

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
  addActions g $ actions lv

  let view  = vView lv
  tag <- newUITag

  view `on` focusInEvent $ do
    liftIO $ mergeUI tag g (Just ui)
    return False

  view `onDestroy` (removeUI $ Just tag)

  return g

actions v =
  [ defAddToPlaylist v
  , defReplacePlaylist v
  , defCopy v
  , defSelectAll v
  , defInvertSelection v
  , defEditProperties v
  , defExportProperties v
  , defSaveCollection v
  , ActionEntry
    { actionEntryName        = "rename-collection"
    , actionEntryLabel       = "Rena_me collection…"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withNames v renameCollection
    }
  , ActionEntry
    { actionEntryName        = "delete-collections"
    , actionEntryLabel       = "_Delete collections"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withNames v deleteCollections
    }
  ]

ui =
  [ ( "ui/view-popup/playlist-actions", playlistActions )
  , ( "ui/menubar/entries/collection/playlist-actions", playlistActions)
  , ( "ui/view-popup/clipboard-actions", clipboardActions )
  , ( "ui/menubar/entries/edit/clipboard-actions", clipboardActions)
  , ( "ui/view-popup/selection-actions", selectionActions )
  , ( "ui/menubar/entries/edit/selection-actions", selectionActions)
  , ( "ui/view-popup/property-actions", propertyActions )
  , ( "ui/menubar/entries/properties/property-actions", propertyActions)
  , ( "ui/view-popup/collection-actions", collectionActions )
  , ( "ui/menubar/entries/collection/collection-actions", collectionActions)
  ]
  where playlistActions =
          [ Just "add-to-playlist"
          , Just "replace-playlist"
          ]
        clipboardActions = [ Just "copy" ]
        selectionActions =
          [ Just "select-all"
          , Just "invert-selection"
          ]
        propertyActions =
          [ Just "edit-properties"
          , Just "export-properties"
          ]
        collectionActions =
          [ Just "save-collection"
          , Just "rename-collection"
          , Just "delete-collections"
          ]
