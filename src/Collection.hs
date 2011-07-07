-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

module Collection
  ( initCollection
  , browseCollection
  ) where

import Control.Monad.ToIO
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (selectAll)

import XMMS2.Client

import UI
import Utils
import Clipboard
import Context
import XMMS

import Collection.List
import Collection.ScrollBox
import Collection.Combo
import qualified Collection.Select as S
import Collection.Actions
import Collection.Utils


initCollection =
  initList

browseCollection _maybeName = do
  builder <- liftIO $ makeBuilder "collection-browser"
  context <- liftIO $ initUI builder
  let ?context = context

  abRef <- liftIO $ newIORef emptyAB

  Just env <- getEnv clipboardEnv
  runEnvT env $ runIn clipboardEnv $> do
    io $ \run ->
      let withColl f = do
            ab <- readIORef abRef
            aWithColl ab f
          withIds f = withColl $ \coll ->
            collQueryIds xmms coll [] 0 0 >>* do
              ids <- result
              liftIO $ f ids
          withSel f = do
            ab <- readIORef abRef
            withJust (aSelection ab) f
      in bindActions builder $
        [ ("add-to-playlist", withColl $ addToPlaylist False)
        , ("replace-playlist", withColl $ addToPlaylist True)
        , ("select-all", withSel selectAll)
        , ("invert-selection", withSel invertSelection)
        , ("copy", withIds (run . copyIds))
        ]

  popup <- liftIO $ getWidget castToMenu "ui/view-popup"

  withListView abRef popup $ do
    view <- listView
    sbox <- liftIO $ mkScrollBox
    cmod <- liftIO $ mkModel
    kill <- getKill
    liftIO $ do
      box    <- builderGetObject builder castToVBox "views"
      scroll <- scrolledWindowNew Nothing Nothing
      scrolledWindowSetShadowType scroll ShadowNone
      scrolledWindowSetPolicy scroll PolicyAutomatic PolicyNever
      boxPackStartDefaults box scroll
      containerAdd scroll $ sViewport sbox
      adj <- scrolledWindowGetHAdjustment scroll
      adj `afterAdjChanged` do
        max <- adjustmentGetUpper adj
        pgs <- adjustmentGetPageSize adj
        adjustmentSetValue adj $ max - pgs
      scroll <- scrolledWindowNew Nothing Nothing
      scrolledWindowSetShadowType scroll ShadowIn
      scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
      scrollBoxAdd sbox scroll
      containerAdd scroll view
    onListSelected $ \coll -> do
      s <- S.mkSelect abRef popup sbox cmod coll
      writeIORef kill $ Just $ S.killSelect s
      scrollBoxAdd sbox $ S.sBox s
      widgetGrabFocus $ S.sCombo s
    return ()

  liftIO $ widgetShowAll window

