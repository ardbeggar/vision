-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
--
--  Copyright (C) 2010 Oleg Belozeorov
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

module Location.View
  ( initView
  , locationView
  , locationSel
  , locationEntry
  ) where

import Data.List
import Data.Char

import Graphics.UI.Gtk

import Env
import Location.Model
import Location.History


data View
  = View { vView  :: TreeView
         , vSel   :: TreeSelection
         , vEntry :: Entry
         }

locationView  = vView getEnv
locationSel   = vSel getEnv
locationEntry = vEntry getEnv


initView = do
  env <- initEnv
  let ?env = env

  treeViewSetRulesHint locationView True

  treeSelectionSetMode locationSel SelectionMultiple

  column <- treeViewColumnNew
  treeViewAppendColumn locationView column
  treeViewColumnSetTitle column "Name"

  cell <- cellRendererPixbufNew
  treeViewColumnPackStart column cell False
  cellLayoutSetAttributeFunc column cell sortModel $ \iter -> do
    item <- itemByIter iter
    cell `set` [ cellPixbufStockId :=
                 if iIsDir item
                 then stockDirectory
                 else stockFile ]

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell sortModel $ \iter -> do
    item <- itemByIter iter
    cell `set` [ cellText := iName item ]

  treeViewSetEnableSearch locationView True
  treeViewSetSearchEqualFunc locationView $ Just $ \str iter -> do
    item <- itemByIter iter
    return $ isInfixOf (map toLower str) (map toLower $ iName item)

  comp <- makeHistoryCompletion
  entrySetCompletion locationEntry comp

  return ?env


initEnv = do
  view  <- treeViewNewWithModel sortModel
  sel   <- treeViewGetSelection view
  entry <- entryNew
  return $ augmentEnv
    View { vView  = view
         , vSel   = sel
         , vEntry = entry
         }

