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
import Control.Monad
import Control.Monad.Trans

import Data.List (intercalate, isInfixOf)
import Data.Char (toLower)
import Data.Map (lookup)

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client as X

import Properties hiding (lookup)
import XMMS
import Utils
import Compound

import Collection.Common
import Collection.Actions
import Collection.Utils


data PropFlt
  = PF { pStore  :: ListStore X.Property
       , pView   :: TreeView
       , pSel    :: TreeSelection
       , pScroll :: ScrolledWindow
       , pColl   :: Coll
       , pProp   :: Property
       }

mkPropFlt env popup prop coll = do
  let abRef = eABRef env
      ae    = eAE env

  store <- listStoreNewDND [] Nothing Nothing
  view  <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionMultiple

  treeViewSetRulesHint view True
  setupTreeViewPopup view popup

  column <- treeViewColumnNew
  treeViewAppendColumn view column
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
        case lookup key p of
          Just s | v == s    -> addLine v ps
                 | otherwise -> do
                   listStoreAppend store s
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

  let pf = PF { pStore  = store
              , pView   = view
              , pSel    = sel
              , pScroll = scroll
              , pColl   = fcoll
              , pProp   = prop
              }
      aef = do
        foc <- view `get` widgetHasFocus
        when foc $ do
          rows <- treeSelectionGetSelectedRows sel
          aEnableSel ae $ not $ null rows
          aEnableRen ae False
          aEnableDel ae False
  setupViewFocus abRef view aef
    AB { aWithColl  = withBuiltColl pf
       , aWithNames = const $ return ()
       , aSelection = Just sel
       }
  sel `on` treeSelectionSelectionChanged $ aef

  return pf

instance CollBuilder PropFlt where
  withBuiltColl pf f = do
    let store = pStore pf
        sel   = pSel pf
    rows <- treeSelectionGetSelectedRows sel
    unless (null rows) $ do
      vals <- mapM (listStoreGetValue store . head) rows
      int  <- collNew TypeIntersection
      collAddOperand int $ pColl pf
      flt <- collParse $ mkFilterText (pProp pf) vals
      collAddOperand int flt
      f int
  treeViewSel pf = (pView pf, pSel pf)

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
