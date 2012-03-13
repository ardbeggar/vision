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

import Prelude hiding (lookup, mapM_)

import Control.Applicative
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.Trans

import Data.Char
import Data.List hiding (lookup)
import Data.Maybe
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

import Graphics.UI.Gtk hiding (selectAll)

import XMMS2.Client

import XMMS
import Utils
import Properties
import Config
import Index hiding (getInfo)
import qualified Index
import Medialib
import Compound
import Clipboard

import Collection.Common
import Collection.Utils


data TrackView
  = TV { tStore   :: ListStore MediaId
       , tIndex   :: Index MediaInfo
       , tSelSet  :: IORef (Set MediaId)
       , tView    :: TreeView
       , tSel     :: TreeSelection
       , tScroll  :: ScrolledWindow
       , tNextRef :: IORef VI
       , tCollRef :: IORef Coll
       , tSetColl :: TrackView -> Coll -> IO ()
       }

instance CollBuilder TrackView where
  withBuiltColl tv s f = do
    let store = tStore tv
        sel   = tSel tv
    rows <- treeSelectionGetSelectedRows sel
    unless (null rows) $ do
      ids <- mapM (listStoreGetValue store . head) rows
      when s $ do
        ss <- readIORef $ tSelSet tv
        let ss' = Set.fromList ids
        writeIORef (tSelSet tv) ss'
        forM_ (Set.union ss ss') $ \id -> do
          Just [r] <- getRefs (tIndex tv) id
          p <- treeRowReferenceGetPath r
          Just iter <- treeModelGetIter store p
          treeModelRowChanged store p iter
      ils <- collNewIdlist ids
      f ils
  treeViewSel tv = (tView tv, tSel tv)


mkTrackView coll = do
  store   <- listStoreNewDND [] Nothing Nothing
  index   <- makeIndex store return
  selSet  <- newIORef Set.empty
  view    <- treeViewNewWithModel store
  sel     <- treeViewGetSelection view
  nextRef <- newIORef None
  collRef <- newIORef coll
  scroll  <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowIn
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  containerAdd scroll view
  let tv = TV { tStore   = store
              , tIndex   = index
              , tSelSet  = selSet
              , tView    = view
              , tSel     = sel
              , tScroll  = scroll
              , tNextRef = nextRef
              , tCollRef = collRef
              , tSetColl = doSetColl
              }
  setupView tv
  setupUI tv
  loadTracks tv
  return tv

instance ViewItem TrackView where
  nextVIRef = tNextRef

setupView tv = do
  let view  = tView tv
      sel   = tSel tv
      popup = coms eVPopup

  treeSelectionSetMode sel SelectionMultiple
  treeViewSetRulesHint view True
  setupTreeViewPopup view popup

  view `onDestroy` (killIndex $ tIndex tv)

  setColumns tv False =<< loadConfig
  widgetShowAll $ tScroll tv

instance SetColl TrackView where
  setColl tv coll = tSetColl tv tv coll

doSetColl tv coll = do
  clearIndex $ tIndex tv
  listStoreClear $ tStore tv
  writeIORef (tSelSet tv) Set.empty
  writeIORef (tCollRef tv) coll
  loadTracks tv

loadTracks tv = do
  coll <- readIORef $ tCollRef tv
  collQueryIds xmms coll [] 0 0 >>* do
    handleXMMSException $ do
      ids <- result
      liftIO $ populateModel tv ids

setColumns tv save props = do
  let view = tView tv
  mapM_ (treeViewRemoveColumn view) =<< treeViewGetColumns view

  column <- treeViewColumnNew
  treeViewAppendColumn (tView tv) column
  cell <- cellRendererToggleNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributes column cell (tStore tv)  $ \i ->
    [ cellToggleActive :=> Set.member i <$> readIORef (tSelSet tv) ]

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


setupUI tv = do
  g <- actionGroupNew "view-actions"

  a <- actionNew "add-to-playlist" "_Add to playlist" Nothing (Just stockAdd)
  actionGroupAddActionWithAccel g a (Just "<Control>Return")
  a `on` actionActivated $ withBuiltColl tv False $ addToPlaylist False

  a <- actionNew "replace-playlist" "_Replace playlist" Nothing Nothing
  actionGroupAddActionWithAccel g a (Just "<Control><Shift>Return")
  a `on` actionActivated $ withBuiltColl tv False $ addToPlaylist True

  a <- actionNew "copy" "_Copy" Nothing (Just stockCopy)
  actionGroupAddActionWithAccel g a (Just "<Control>c")
  a `on` actionActivated $ withBuiltColl tv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ copyIds ids

  a <- actionNew "select-all" "_Select all" Nothing (Just stockSelectAll)
  actionGroupAddActionWithAccel g a (Just "<Control>a")
  a `on` actionActivated $ selectAll $ tSel tv

  a <- actionNew "invert-selection" "_Invert selection" Nothing (Just stockSelectAll)
  actionGroupAddActionWithAccel g a (Just "<Control><Shift>a")
  a `on` actionActivated $ invertSelection $ tSel tv

  a <- actionNew "edit-properties" "_Edit properties" Nothing (Just stockEdit)
  actionGroupAddActionWithAccel g a (Just "<Alt>Return")
  a `on` actionActivated $ withBuiltColl tv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyEditor ids

  a <- actionNew "export-properties" "E_xport properties…" Nothing (Just stockSave)
  actionGroupAddActionWithAccel g a (Just "")
  a `on` actionActivated $ withBuiltColl tv False $ \coll -> do
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ showPropertyExport ids

  a <- actionNew "save-collection" "_Save collection…" Nothing (Just stockSave)
  actionGroupAddActionWithAccel g a (Just "<Control>s")
  a `on` actionActivated $ withBuiltColl tv False saveCollection

  let view  = tView tv
  tag <- newUITag

  view `on` focusInEvent $ do
    liftIO $ mergeUI tag g (Just ui)
    return False

  view `onDestroy` (removeUI $ Just tag)

  return ()

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
        collectionActions = [ Just "save-collection" ]
