-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2011
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

module Collection.PropFlt
  ( PropFlt (..)
  , mkPropFlt
  ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad (when, unless)
import Control.Monad.Trans

import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import Data.Foldable

import Graphics.UI.Gtk hiding (selectAll)

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client as X

import Properties hiding (lookup)
import XMMS
import Utils
import Compound
import Clipboard

import Collection.Common
import Collection.Utils


deriving instance Ord X.Property


data PropFlt
  = PF { pStore   :: ListStore X.Property
       , pSelIx   :: IORef (Map X.Property TreeRowReference)
       , pSelSet  :: IORef (Set X.Property)
       , pView    :: TreeView
       , pSel     :: TreeSelection
       , pScroll  :: ScrolledWindow
       , pCollRef :: IORef Coll
       , pProp    :: Property
       , pNextRef :: IORef VI
       , pSetColl :: PropFlt -> Coll -> IO ()
       , pLoadRef :: IORef Int
       }

instance ViewItem PropFlt where
  nextVIRef = pNextRef

mkPropFlt prop coll = do
  let popup = coms eVPopup

  selIx  <- newIORef Map.empty
  selSet <- newIORef Set.empty

  store <- listStoreNewDND [] Nothing Nothing
  view  <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionMultiple

  treeViewSetRulesHint view True
  setupTreeViewPopup view popup

  column <- treeViewColumnNew
  treeViewAppendColumn view column

  cell <- cellRendererToggleNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributes column cell store $ \p ->
    [ cellToggleActive :=> Set.member p <$> readIORef selSet ]

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \p ->
    [ cellText := showValue prop p ]

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowIn
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  containerAdd scroll view
  widgetShowAll scroll

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view . Just $ \str iter ->
    (isInfixOf (map toLower str) . map toLower . showValue prop) <$>
    (listStoreGetValue store $ listStoreIterToIndex iter)

  collRef <- newIORef coll
  nextRef <- newIORef None
  loadRef <- newIORef 0

  let pf = PF { pStore   = store
              , pSelIx   = selIx
              , pSelSet  = selSet
              , pView    = view
              , pSel     = sel
              , pScroll  = scroll
              , pCollRef = collRef
              , pProp    = prop
              , pNextRef = nextRef
              , pSetColl = doSetColl
              , pLoadRef = loadRef
              }
  loadColl pf
  setupUI pf
  return pf

instance SetColl PropFlt where
  setColl pf coll = pSetColl pf pf coll

doSetColl pf coll = do
  modifyIORef (pLoadRef pf) (+ 1)
  listStoreClear $ pStore pf
  writeIORef (pSelIx pf) Map.empty
  writeIORef (pSelSet pf) Set.empty
  writeIORef (pCollRef pf) coll
  loadColl pf

loadColl pf = do
  let key = propKey $ pProp pf

  fcoll <- collNew TypeIntersection
  coll <- readIORef $ pCollRef pf
  collAddOperand fcoll coll
  flt <- collNew TypeHas
  collAttributeSet flt "field" key
  collAddOperand flt =<< collUniverse
  collAddOperand fcoll flt

  load <- readIORef $ pLoadRef pf

  let store = pStore pf
      selIx = pSelIx pf
      addLine v [] = return v
      addLine v (p : ps) =
        case Map.lookup key p of
          Just s | v == s    -> addLine v ps
                 | otherwise -> do
                   n      <- listStoreAppend store s
                   Just r <- treeRowReferenceNew store [n]
                   modifyIORef selIx $ Map.insert s r
                   addLine s ps
          Nothing -> addLine v ps
      getInfos s v = do
        load' <- readIORef $ pLoadRef pf
        when (load' == load) $
          collQueryInfos xmms fcoll [key] s 100 [key] [key] >>* do
            handleXMMSException $ do
              lst <- result
              len <- resultLength
              liftIO $ do
                v' <- addLine v lst
                when (len == 100) $
                  getInfos (s + 100) v'
  getInfos 0 (PropString "")


instance CollBuilder PropFlt where
  withBuiltColl pf s f = do
    let store = pStore pf
        sel   = pSel pf
    rows <- treeSelectionGetSelectedRows sel
    unless (null rows) $ do
      vals <- mapM (listStoreGetValue store . head) rows
      when s $ do
        ss <- readIORef $ pSelSet pf
        let ss' = Set.fromList vals
        writeIORef (pSelSet pf) ss'
        ix <- readIORef $ pSelIx pf
        forM_ (Set.union ss ss') $ \p ->
          withJust (Map.lookup p ix) $ \r -> do
            p <- treeRowReferenceGetPath r
            Just iter <- treeModelGetIter store p
            treeModelRowChanged store p iter
      int <- collNew TypeIntersection
      collAddOperand int =<< readIORef (pCollRef pf)
      flt <- mkFilter (pProp pf) vals
      collAddOperand int flt
      f int
  treeViewSel pf    = (pView pf, pSel pf)


instance CompoundWidget PropFlt where
  type Outer PropFlt = ScrolledWindow
  outer = pScroll

instance FocusChild PropFlt where
  type Focus PropFlt = TreeView
  focus = pView

mkFilter :: Property -> [X.Property] -> IO Coll
mkFilter prop list = do
  uni <- collNew TypeUnion
  add uni list
  return uni
  where add _ []     = return ()
        add uni list = do
          let (h, t) = splitAt 100 list
          flt <- collNew TypeUnion
          forM_ h $ \v -> do
            tmp <- collNew TypeEquals
            collAddOperand tmp =<< collUniverse
            collAttributeSet tmp "field" $ propKey prop
            collAttributeSet tmp "value" $ case v of
              PropString s -> s
              PropInt32  i -> show i
            collAddOperand flt tmp
          collAddOperand uni flt
          add uni t


setupUI pf = do
  g <- actionGroupNew "view-actions"
  addActions g $ actions pf

  let view  = pView pf
  tag <- newUITag

  view `on` focusInEvent $ do
    liftIO $ mergeUI tag g (Just ui)
    return False

  view `onDestroy` (removeUI $ Just tag)

  return ()

actions pf =
  [ ActionEntry
    { actionEntryName        = "add-to-playlist"
    , actionEntryLabel       = "_Add to playlist"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Just "<Control>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False $ addToPlaylist False
    }
  , ActionEntry
    { actionEntryName        = "replace-playlist"
    , actionEntryLabel       = "_Replace playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Just "<Control><Shift>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False $ addToPlaylist True
    }
  , ActionEntry
    { actionEntryName        = "copy"
    , actionEntryLabel       = "_Copy"
    , actionEntryStockId     = Just stockCopy
    , actionEntryAccelerator = Just "<Control>c"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False $ \coll ->
      collQueryIds xmms coll [] 0 0 >>* do
        ids <- result
        liftIO $ copyIds ids
    }
  , ActionEntry
    { actionEntryName        = "select-all"
    , actionEntryLabel       = "_Select all"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = selectAll $ pSel pf
    }
  , ActionEntry
    { actionEntryName        = "invert-selection"
    , actionEntryLabel       = "_Invert selection"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control><Shift>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = invertSelection $ pSel pf
    }
  , ActionEntry
    { actionEntryName        = "edit-properties"
    , actionEntryLabel       = "_Edit properties"
    , actionEntryStockId     = Just stockEdit
    , actionEntryAccelerator = Just "<Alt>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False $ \coll ->
      collQueryIds xmms coll [] 0 0 >>* do
        ids <- result
        liftIO $ showPropertyEditor ids
    }
  , ActionEntry
    { actionEntryName        = "export-properties"
    , actionEntryLabel       = "E_xport properties"
    , actionEntryStockId     = Just stockSave
    , actionEntryAccelerator = Just ""
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False $ \coll ->
      collQueryIds xmms coll [] 0 0 >>* do
        ids <- result
        liftIO $ showPropertyExport ids
    }
  , ActionEntry
    { actionEntryName        = "save-collection"
    , actionEntryLabel       = "_Save collection…"
    , actionEntryStockId     = Just stockSave
    , actionEntryAccelerator = Just "<Control>s"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = withBuiltColl pf False saveCollection
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
        collectionActions = [ Just "save-collection" ]
