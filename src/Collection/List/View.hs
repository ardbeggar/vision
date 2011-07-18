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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Control.Applicative
import Control.Monad hiding (forM_)

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.IORef
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Utils
import Compound

import Collection.Common
import Collection.List.Model
import Collection.Actions
import Collection.Utils


data ListView
  = V { vView    :: TreeView
      , vSel     :: TreeSelection
      , vNextRef :: IORef VI
      , vStore   :: ListStore (Maybe (String, Coll))
      , vIndex   :: IORef (Map String TreeRowReference)
      , vSelSet  :: IORef (Set (Maybe String))
      , vScroll  :: ScrolledWindow
      }

mkListView = withModel $ do
  let abRef = coms eABRef
      ae    = coms eAE
      popup = coms eLPopup

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

  cell <- cellRendererPixbufNew
  cell `set` [ cellWidth := 30 ]
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributes column cell store $ \n ->
    [ cellPixbufStockId :=> do
         ss <- readIORef selSet
         if Set.member (maybe Nothing (Just . fst) n) ss
           then return stockApply
           else return ""
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
      aef = do
        foc <- view `get` widgetHasFocus
        when foc $ do
          rows <- treeSelectionGetSelectedRows sel
          aEnableSel ae $ not $ null rows
          aEnableDel ae $ case rows of
            []      -> False
            [0] : _ -> False
            _       -> True
          aEnableRen ae $ case rows of
            [[0]] -> False
            [_]   -> True
            _     -> False
  setupViewFocus abRef view aef
    AB { aWithColl  = withBuiltColl v
       , aWithNames = \f -> withColls v (f . map fst . catMaybes)
       , aSelection = Just sel
       }
  sel `on` treeSelectionSelectionChanged $ aef

  xcW <- atomically $ newTGWatch connectedV
  tid <- forkIO $ forever $ do
    void $ atomically $ watch xcW
    postGUISync $ killNext v
  view `onDestroy` (killThread tid)

  widgetShowAll scroll
  return v

instance CollBuilder ListView where
  withBuiltColl lv f = withColls lv $ withColl lv f
  treeViewSel lv = (vView lv, vSel lv)

withColls lv f = do
  let store = vStore lv
      sel   = vSel lv
  rows <- treeSelectionGetSelectedRows sel
  unless (null rows) $ do
    colls <- mapM (listStoreGetValue store . head) rows
    f colls

withColl v f list = do
  ss <- readIORef $ vSelSet v
  writeIORef (vSelSet v) Set.empty
  ix <- readIORef $ vIndex v
  Just iter <- treeModelGetIter (vStore v) [0]
  treeModelRowChanged (vStore v) [0] iter
  forM_ ss $ \mn ->
    withJust mn $ \name ->
      withJust (Map.lookup name ix) $ \r -> do
        p <- treeRowReferenceGetPath r
        Just iter <- treeModelGetIter (vStore v) p
        treeModelRowChanged (vStore v) p iter
  withColl' v f list

withColl' _ _ []            = return ()
withColl' v f (Nothing : _) = do
  writeIORef (vSelSet v) $ Set.singleton Nothing
  Just iter <- treeModelGetIter (vStore v) [0]
  treeModelRowChanged (vStore v) [0] iter
  f =<< collUniverse
withColl' v f list = do
  let ss = Set.fromList $ map (Just . fst) $ catMaybes list
  writeIORef (vSelSet v) ss
  ix <- readIORef $ vIndex v
  uni <- collNew TypeUnion
  forM_ (catMaybes list) $ \c -> do
    collAddOperand uni $ snd c
    withJust (Map.lookup (fst c) ix) $ \r -> do
      p <- treeRowReferenceGetPath r
      Just iter <- treeModelGetIter (vStore v) p
      treeModelRowChanged (vStore v) p iter
  f uni

instance CompoundWidget ListView where
  type Outer ListView = ScrolledWindow
  outer = vScroll

instance FocusChild ListView where
  type Focus ListView = TreeView
  focus = vView

instance ViewItem ListView where
  nextVIRef = vNextRef