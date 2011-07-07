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
  , onPropsSelected
  ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef
import Data.List (intercalate, isInfixOf)
import Data.Char (toLower)

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)

import Properties
import XMMS
import Utils

import Collection.Actions


data PropFlt
  = PF { pStore  :: ListStore String
       , pView   :: TreeView
       , pScroll :: ScrolledWindow
       , pColl   :: Coll
       , pProp   :: Property
       }

mkPropFlt abRef prop coll = do
  store <- listStoreNewDND [] Nothing Nothing
  view  <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionMultiple

  column <- treeViewColumnNew
  treeViewAppendColumn view column
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \n ->
    [ cellText := n ]

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  containerAdd scroll view
  widgetShowAll scroll

  treeViewSetEnableSearch view True
  treeViewSetSearchEqualFunc view . Just $ \str iter ->
    (isInfixOf (map toLower str) . map toLower) <$>
    (listStoreGetValue store $ listStoreIterToIndex iter)

  fcoll <- collNew TypeIntersection
  collAddOperand fcoll coll
  flt <- collParse $ "NOT " ++ propKey prop ++ ":''"
  collAddOperand fcoll flt

  let key = propKey prop
      addLine v [] = return v
      addLine v (p : ps) =
        case lookup prop p of
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
  getInfos 0 ""

  let pf = PF { pStore  = store
              , pView   = view
              , pScroll = scroll
              , pColl   = fcoll
              , pProp   = prop
              }
      doAdd replace = do
        rows <- treeSelectionGetSelectedRows sel
        unless (null rows) $ do
          vals <- mapM (listStoreGetValue store . head) rows
          int  <- collNew TypeIntersection
          collAddOperand int $ pColl pf
          flt <- collParse $ mkFilterText (pProp pf) vals
          collAddOperand int flt
          addToPlaylist replace int

  view `on` focusInEvent $ liftIO $ do
    writeIORef abRef
      AB { aAdd = doAdd False
         , aReplace = doAdd True
         , aSelection = Just sel
         }
    return False

  return pf

onPropsSelected pf f = do
  let store = pStore pf
      view  = pView pf
  sel <- treeViewGetSelection view
  let doit = do
        rows <- treeSelectionGetSelectedRows sel
        unless (null rows) $ do
          vals <- mapM (listStoreGetValue store . head) rows
          int  <- collNew TypeIntersection
          collAddOperand int $ pColl pf
          flt <- collParse $ mkFilterText (pProp pf) vals
          collAddOperand int flt
          f int
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

cond' [] = "'"
cond' ('\'' : t) = '\\' : '\'' : cond' t
cond' ('\\' : t) = '\\' : '\\' : cond' t
cond' (h : t) = h : cond' t

cond prop val
  | propKey prop == "url" = "url:'" ++ encodeURL val ++ "'"
  | otherwise             = propKey prop ++ ":\'" ++ cond' val

mkFilterText prop vals =
  intercalate " OR " $ map (cond prop) vals

addToPlaylist replace coll = do
  when replace $ playlistClear xmms Nothing >> return ()
  playlistAddCollection xmms Nothing coll []
  return ()
