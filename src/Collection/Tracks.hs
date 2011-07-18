-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 5 Jul. 2011
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

module Collection.Tracks
  ( TrackView (..)
  , mkTrackView
  ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Char
import Data.List hiding (lookup)
import Data.Maybe
import Data.IORef

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Utils
import Properties
import Config
import Index hiding (getInfo)
import qualified Index
import Medialib
import Compound

import Collection.Common
import Collection.Actions
import Collection.Utils


data TrackView
  = TV { tStore   :: ListStore MediaId
       , tIndex   :: Index MediaInfo
       , tView    :: TreeView
       , tSel     :: TreeSelection
       , tScroll  :: ScrolledWindow
       , tNextRef :: IORef VI
       }

instance CollBuilder TrackView where
  withBuiltColl tv f = do
    let store = tStore tv
        sel   = tSel tv
    rows <- treeSelectionGetSelectedRows sel
    unless (null rows) $ do
      ids <- mapM (listStoreGetValue store . head) rows
      ils <- collNewIdlist ids
      f ils
  treeViewSel tv    = (tView tv, tSel tv)
  actionBackend tv  =
    AB { aWithColl  = withBuiltColl tv
       , aWithNames = const $ return ()
       , aSelection = Just $ tSel tv
       }


mkTrackView coll = do
  store   <- listStoreNewDND [] Nothing Nothing
  index   <- makeIndex store return
  view    <- treeViewNewWithModel store
  sel     <- treeViewGetSelection view
  nextRef <- newIORef None
  scroll  <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowIn
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  containerAdd scroll view
  let tv = TV { tStore   = store
              , tIndex   = index
              , tView    = view
              , tSel     = sel
              , tScroll  = scroll
              , tNextRef = nextRef
              }
  setupView tv
  loadTracks tv coll
  return tv

instance ViewItem TrackView where
  nextVIRef = tNextRef

setupView tv = do
  let view  = tView tv
      sel   = tSel tv
      ae    = coms eAE
      popup = coms eVPopup

  treeSelectionSetMode sel SelectionMultiple
  treeViewSetRulesHint view True
  setupTreeViewPopup view popup

  let aef = do
        foc <- view `get` widgetHasFocus
        when foc $ do
          rows <- treeSelectionGetSelectedRows sel
          aEnableSel ae $ not $ null rows
          aEnableRen ae False
          aEnableDel ae False
  setupViewFocus tv aef
  sel `on` treeSelectionSelectionChanged $ aef

  view `onDestroy` (killIndex $ tIndex tv)

  setColumns tv False =<< loadConfig
  widgetShowAll $ tScroll tv

loadTracks tv coll =
  collQueryIds xmms coll [] 0 0 >>* do
    ids <- result
    liftIO $ populateModel tv ids

setColumns tv save props = do
  let view = tView tv
  mapM_ (treeViewRemoveColumn view) =<< treeViewGetColumns view
  mapM_ (addColumn tv) props
  setupSearch tv props
  when save $ saveConfig props

addColumn tv prop = do
  let view  = tView tv
      store = tStore tv
  column <- treeViewColumnNew
  treeViewAppendColumn view column
  treeViewColumnSetTitle column $ propName prop
  treeViewColumnSetResizable column True
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell store $ \iter -> do
    maybeInfo <- getInfoIfNeeded tv iter
    let text = case maybeInfo of
          Just info -> fromMaybe "" $ lookup prop info
          Nothing   -> ""
    cell `set` [ cellText := text ]

getInfoIfNeeded tv iter = do
  let n = listStoreIterToIndex iter
  mid <- listStoreGetValue (tStore tv) n
  rng <- treeViewGetVisibleRange $ tView tv
  getInfo tv mid $ case rng of
    ([f], [t]) | n >= f && t >= n -> Visible
    _                             -> Background

loadConfig =
  catMaybes <$> (mapM property =<< config configFile defaultConfig)

saveConfig props = do
  writeConfig configFile $ map propName props
  return ()

configFile =
  "collection-view.conf"

defaultConfig =
  ["Artist", "Album", "Track", "Title"]

setupSearch tv props = do
  let store = tStore tv
      view  = tView tv
  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    mid <- listStoreGetValue store $ listStoreIterToIndex iter
    search (map toLower str) props <$> getInfo tv mid Search

search _ _ Nothing = False
search _ [] _ = False
search str (prop:props) (Just info) =
  let ptext = map toLower $ fromMaybe "" $ lookup prop info in
  str `isInfixOf` ptext || search str props (Just info)

getInfo tv = Index.getInfo (tIndex tv)

populateModel tv ids = do
  mapM_ addOne ids
  where addOne id = do
          n <- listStoreAppend store id
          addToIndex index id n
        store = tStore tv
        index = tIndex tv

instance CompoundWidget TrackView where
  type Outer TrackView = ScrolledWindow
  outer = tScroll

instance FocusChild TrackView where
  type Focus TrackView = TreeView
  focus = tView
