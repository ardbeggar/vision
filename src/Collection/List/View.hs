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

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Collection.List.View
  ( withListView
  , listView
  , getKill
  , onListSelected
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.ReaderX

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Typeable
import Data.IORef

import Graphics.UI.Gtk

import XMMS2.Client

import Context
import XMMS
import Utils

import Collection.List.Model
import Collection.Actions
import Collection.Utils


data Ix = Ix deriving (Typeable)
instance Index Ix where getVal = Ix

data View
  = View { vView :: TreeView
         , vSel  :: TreeSelection
         , vKill :: IORef (Maybe (IO ()))
         }
  deriving (Typeable)

listView = asksx Ix vView

withListView abRef popup m = do
  Just env <- getEnv modelEnv
  runEnvT env $ do
    store <- store
    view  <- makeView abRef popup store
    runEnvT (Ix, view) m

makeView abRef popup store = liftIO $ do
  view <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False

  column <- treeViewColumnNew
  treeViewAppendColumn view column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \n ->
    case n of
      Nothing ->
        [ cellText := "All Media", cellTextWeight := 800 ]
      Just cn ->
        [ cellText := cn, cellTextWeightSet := False ]

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view . Just $ \str iter ->
    maybe False (isInfixOf (map toLower str) . map toLower) <$>
      (listStoreGetValue store $ listStoreIterToIndex iter)

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionMultiple

  treeViewSetRulesHint view True
  setupTreeViewPopup view popup

  let doAdd replace = do
        rows <- treeSelectionGetSelectedRows sel
        unless (null rows) $ do
          names <- mapM (listStoreGetValue store . head) rows
          withColl (addToPlaylist replace) names

  setupViewFocus abRef view
    AB { aAdd       = doAdd False
       , aReplace   = doAdd True
       , aSelection = Just sel
       }

  kill <- newIORef Nothing

  return View { vView = view, vSel = sel, vKill = kill }

onListSelected f = do
  sel   <- asksx Ix vSel
  view  <- asksx Ix vView
  kill  <- asksx Ix vKill
  store <- store
  liftIO $ do
    let doit = do
          rows <- treeSelectionGetSelectedRows sel
          unless (null rows) $ do
            maybeKill <- readIORef kill
            withJust maybeKill $ \kill -> kill
            writeIORef kill Nothing
            names <- mapM (listStoreGetValue store . head) rows
            withColl f names
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

withColl f list = do
  if Nothing `elem` list
    then do
    coll <- collUniverse
    f coll
    else do
    uni <- collNew TypeUnion
    withUni f uni list

withUni f uni [] = f uni
withUni f uni ((Just name) : names) =
  collGet xmms name "Collections" >>* do
    coll <- result
    liftIO $ do
      collAddOperand uni coll
      withUni f uni names

getKill = asksx Ix vKill
