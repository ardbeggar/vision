-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

module Collection.View
  ( initView
  , collView
  , collSel
  , collFilter
  , showViewConfigDialog
  ) where

import Control.Monad
import Control.Applicative

import Data.IORef
import Data.Maybe

import System.IO.Unsafe

import Graphics.UI.Gtk

import UI
import Config
import Compound
import Editor
import Context
import Prelude hiding (lookup)
import Properties
import Collection.Model


data View
  = View { vView      :: TreeView
         , vSel       :: TreeSelection
         , vFilter    :: Entry
         , vConfigDlg :: EditorDialog (PropertyView ())
         , vColumns   :: IORef [Property]
         }

collView   = vView context
collSel    = vSel  context
collFilter = vFilter context
configDlg  = vConfigDlg context
columns    = vColumns context


initView = do
  context <- initContext
  let ?context = context

  treeViewSetRulesHint collView True
  treeSelectionSetMode collSel SelectionMultiple

  setColumns =<< loadConfig

  return ?context

showViewConfigDialog =
  runEditorDialog configDlg
  (map (, ()) <$> getColumns)
  (setColumns . map fst)
  False window


initContext = do
  view      <- treeViewNewWithModel collStore
  sel       <- treeViewGetSelection view
  filter    <- entryNew
  configDlg <- unsafeInterleaveIO $ makeConfigDlg
  columns   <- newIORef []
  return $ augmentContext
    View { vView      = view
         , vSel       = sel
         , vFilter    = filter
         , vConfigDlg = configDlg
         , vColumns   = columns
         }

getColumns =
  readIORef columns

setColumns props = do
  mapM_ (treeViewRemoveColumn collView) =<< treeViewGetColumns collView
  writeIORef columns props
  mapM_ addColumn props
  saveConfig props

addColumn prop = do
  column <- treeViewColumnNew
  treeViewAppendColumn collView column
  treeViewColumnSetTitle column $ propName prop
  treeViewColumnSetResizable column True
  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributeFunc column cell collStore $ \iter -> do
    maybeInfo <- getInfoIfNeeded iter
    let text = case maybeInfo of
          Just info -> maybe "" id (lookup prop info)
          Nothing   -> ""
    cell `set` [ cellText := text ]

getInfoIfNeeded iter = do
  [n] <- treeModelGetPath collStore iter
  mid <- listStoreGetValue collStore n
  rng <- treeViewGetVisibleRange collView
  getInfo mid $ case rng of
    ([f], [t]) -> n >= f && t >= n
    _          -> False

makeConfigDlg =
  makeEditorDialog [(stockApply, ResponseApply)]
  makePropertyView $ \v -> do
    let outerw = outer v
    windowSetTitle outerw "Configure collection browser"
    windowSetDefaultSize outerw 500 400

loadConfig =
  catMaybes <$> (mapM property =<< config configFile defaultConfig)

saveConfig props = do
  writeConfig configFile $ map propName props
  return ()

configFile =
  "collection-view.conf"

defaultConfig =
  ["Artist", "Album", "Track", "Title"]
