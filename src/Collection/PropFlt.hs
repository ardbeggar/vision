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

import Data.List (intercalate, isInfixOf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import Data.Foldable

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client as X

import Properties hiding (lookup)
import XMMS
import Utils
import Compound

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
       , pColl    :: Coll
       , pProp    :: Property
       , pNextRef :: IORef VI
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

  fcoll <- collNew TypeIntersection
  collAddOperand fcoll coll
  flt <- collParse $ "NOT " ++ propKey prop ++ ":''"
  collAddOperand fcoll flt

  let key = propKey prop
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
      getInfos s v =
        collQueryInfos xmms fcoll [key] s 100 [key] [key] >>* do
          lst <- result
          len <- resultLength
          liftIO $ do
            v' <- addLine v lst
            when (len == 100) $
              getInfos (s + 100) v'
  getInfos 0 (PropString "")

  nextRef <- newIORef None

  let pf = PF { pStore   = store
              , pSelIx   = selIx
              , pSelSet  = selSet
              , pView    = view
              , pSel     = sel
              , pScroll  = scroll
              , pColl    = fcoll
              , pProp    = prop
              , pNextRef = nextRef
              }
  setupViewFocus pf
  return pf

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
      collAddOperand int $ pColl pf
      flt <- collParse $ mkFilterText (pProp pf) vals
      collAddOperand int flt
      f int
  treeViewSel pf    = (pView pf, pSel pf)


instance CompoundWidget PropFlt where
  type Outer PropFlt = ScrolledWindow
  outer = pScroll

instance FocusChild PropFlt where
  type Focus PropFlt = TreeView
  focus = pView


cond' [] = "'"
cond' ('\'' : t) = '\\' : '\'' : cond' t
cond' ('\\' : t) = '\\' : '\\' : cond' t
cond' (h : t) = h : cond' t

cond prop (PropString s)
  | propKey prop == "url" = "url:'" ++ s ++ "'"
  | otherwise             = propKey prop ++ ":'" ++ cond' s
cond prop (PropInt32 i)   = propKey prop ++ ":" ++ show i

mkFilterText prop vals =
  intercalate " OR " $ map (cond prop) vals
