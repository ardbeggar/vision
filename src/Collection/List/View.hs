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
import Control.Monad.Trans
import Control.Monad.ReaderX

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.IORef
import Data.Maybe

import Graphics.UI.Gtk

import XMMS2.Client

import Context
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
      , vScroll  :: ScrolledWindow
      }

mkListView env = do
  Just me <- getEnv modelEnv
  store   <- runEnvT me store
  liftIO $ do
    let abRef = eABRef env
        ae    = eAE env
    view <- treeViewNewWithModel store
    treeViewSetHeadersVisible view False
    treeViewSetRulesHint view True

    sel <- treeViewGetSelection view
    treeSelectionSetMode sel SelectionMultiple
    setupTreeViewPopup view $ eLPopup env

    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType scroll ShadowIn
    scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
    containerAdd scroll view

    column <- treeViewColumnNew
    treeViewAppendColumn view column
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
              , vScroll  = scroll
              }

    let aef = do
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
  withBuiltColl lv f = withColls lv $ withColl f
  treeViewSel lv = (vView lv, vSel lv)

withColls lv f = do
  let store = vStore lv
      sel   = vSel lv
  rows <- treeSelectionGetSelectedRows sel
  unless (null rows) $ do
    colls <- mapM (listStoreGetValue store . head) rows
    f colls

withColl _ []            = return ()
withColl f (Nothing : _) = f =<< collUniverse
withColl f list = do
  uni <- collNew TypeUnion
  mapM_ (collAddOperand uni . snd) $ catMaybes list
  f uni

instance CompoundWidget ListView where
  type Outer ListView = ScrolledWindow
  outer = vScroll

instance FocusChild ListView where
  type Focus ListView = TreeView
  focus = vView

instance ViewItem ListView where
  nextVIRef = vNextRef