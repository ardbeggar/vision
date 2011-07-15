-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Playlist.UI
  ( setupUI
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.W
import Control.Monad.ToIO
import Control.Monad.EnvIO

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch
import Control.Concurrent.STM.TGVar

import System.IO.Unsafe

import Network.URL

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client hiding (Data, playbackStatus)

import Builder
import UI
import XMMS
import Playback
import Playtime
import Volume
import Utils
import Clipboard
import Location
import Collection
import Compound
import Editor
import Registry
import Properties hiding (showPropertyEditor, showPropertyExport)
import Playlist.Model
import Playlist.View
import Playlist.Edit
import Playlist.Config
import Playlist.Control


setupUI = do
  setupWindowTitle

  Just ptEnv  <- getEnv playtimeEnv
  Just volEnv <- getEnv volumeEnv
  runIn (ptEnv :*: volEnv) $> setupPlaybar

  Just env <- getEnv clipboardEnv
  runIn (env :*: registryEnv) $> setupActions

  popup <- getWidget castToMenu "ui/playlist-popup"
  liftIO $ setupTreeViewPopup playlistView popup

  liftIO $ do
    playlistView `onRowActivated` \[n] _ ->
      playTrack n
    window `onDestroy` mainQuit

  return ()

setupActions = do
  urlEntryDialog <- liftIO $ unsafeInterleaveIO $ makeURLEntryDialog

  orderDialog <- liftIO $ unsafeInterleaveIO $ makeOrderDialog $ \v -> do
    let outerw = outer v
    windowSetDefaultSize outerw 500 400
    nw  <- atomically $ newEmptyTWatch playlistName
    tid <- forkIO $ forever $ do
      name <- atomically $ watch nw
      windowSetTitle outerw $
        "Sort playlist" ++ maybe "" (": " ++) name
    outerw `onDestroy` (killThread tid)

  W runR <- runIn registryEnv
  W runC <- runIn clipboardEnv
  bindActions
    [ ("play",               startPlayback False)
    , ("pause",              pausePlayback)
    , ("stop",               stopPlayback)
    , ("prev",               prevTrack)
    , ("next",               nextTrack)
    , ("quit",               mainQuit)
    , ("cut",                runC $ editDelete True)
    , ("copy",               runC $ editCopy)
    , ("paste",              runC $ editPaste False)
    , ("append",             runC $ editPaste True)
    , ("delete",             runC $ editDelete False)
    , ("select-all",         editSelectAll)
    , ("invert-selection",   editInvertSelection)
    , ("browse-location",    browseLocation SortAscending Nothing)
    , ("browse-collection",  runR $ browseCollection Nothing)
    , ("add-media",          runURLEntryDialog urlEntryDialog)
    , ("clear-playlist",     clearPlaylist)
    , ("sort-by",            showOrderDialog orderDialog getOrder setOrder)
    , ("configure-playlist", showPlaylistConfigDialog)
    , ("edit-properties",    showPropertyEditor)
    , ("export-properties",  showPropertyExport)
    , ("import-properties",  showPropertyImport)
    , ("manage-properties",  showPropertyManager)
    ]

  setupServerActionGroup
  setupPlaybackActions
  setupTrackActions
  setupSelectionActions
  setupClipboardActions

  return ()

setupServerActionGroup = do
  ag <- getObject castToActionGroup "server-actions"
  liftIO $ do
    cW <- atomically $ newTGWatch connectedV
    forkIO $ forever $ do
      c <- atomically $ watch cW
      actionGroupSetSensitive ag c

setupPlaybackActions = do
  play  <- action "play"
  pause <- action "pause"
  stop  <- action "stop"
  liftIO $ do
    psW   <- atomically $ newEmptyTWatch playbackStatus
    forkIO $ forever $ do
      ps <- atomically $ watch psW
      postGUISync $ do
        let (ePlay, ePause, eStop) = case ps of
              Just StatusPlay  -> (False, True, True)
              Just StatusPause -> (True, False, True)
              _                -> (True, False, False)
        actionSetSensitive play ePlay
        actionSetVisible play ePlay
        actionSetSensitive pause ePause
        actionSetVisible pause ePause
        actionSetSensitive stop eStop

setupTrackActions = do
  prev <- action "prev"
  next <- action "next"
  liftIO $ do
    ctW  <- atomically $ newEmptyTWatch currentTrack
    psW  <- atomically $ newEmptyTWatch playlistSize
    forkIO $ forever $ do
      atomically $ (void $ watch ctW) `mplus` (void $ watch psW)
      size <- getPlaylistSize
      name <- getPlaylistName
      cpos <- getCurrentTrack
      let (ep, en) = case (name, cpos) of
            (Just n, Just (ct, cn)) ->
              (n == cn && ct > 0, n == cn && ct < size - 1)
            _ ->
              (False, False)
      actionSetSensitive prev ep
      actionSetSensitive next en

setupSelectionActions = do
  acts <- actions ["cut", "copy", "delete", "edit-properties", "export-properties"]
  liftIO $ do
    playlistSel `onSelectionChanged` setup acts
    postGUIAsync $ setup acts
  where setup acts = do
          n <- treeSelectionCountSelectedRows playlistSel
          mapM_ (`actionSetSensitive` (n /= 0)) acts

setupClipboardActions = do
  acts <- actions ["paste", "append"]
  runIn clipboardEnv $> do
    ts <- clipboardTargets
    io $ \run -> do
      ctW <- atomically $ newEmptyTWatch ts
      forkIO $ forever $ do
        void $ atomically $ watch ctW
        en <- run editCheckClipboard
        mapM_ (`actionSetSensitive` en) acts

setupWindowTitle = io $ \run -> do
  pnW <- atomically $ newEmptyTWatch playlistName
  forkIO $ forever $ do
    name <- atomically $ watch pnW
    run $ setWindowTitle $
      maybe "Vision playlist" (++ " - Vision playlist") name

setupPlaybar = do
  seekView <- makeSeekControl
  volView  <- makeVolumeControl
  playbar  <- getObject castToToolbar "playbar"
  liftIO $ do
    sep <- separatorToolItemNew
    separatorToolItemSetDraw sep False
    toolbarInsert playbar sep (-1)

    seekItem <- toolItemNew
    toolItemSetHomogeneous seekItem False
    toolItemSetExpand seekItem True
    containerAdd seekItem seekView
    toolbarInsert playbar seekItem (-1)

    sep <- separatorToolItemNew
    separatorToolItemSetDraw sep False
    toolbarInsert playbar sep (-1)

    volumeItem <- toolItemNew
    toolItemSetHomogeneous volumeItem False
    toolItemSetExpand volumeItem False
    widgetSetSizeRequest volumeItem 100 (-1)
    containerAdd volumeItem volView
    toolbarInsert playbar volumeItem (-1)

  return ()



data URLEntry =
  URLEntry { urlEntry :: Entry
           , urlBox   :: HBox
           }

instance CompoundWidget URLEntry where
  type Outer URLEntry = HBox
  outer = urlBox

instance EditorWidget URLEntry where
  type Data URLEntry = String
  setData e     = entrySetText (urlEntry e)
  getData       = entryGetText . urlEntry
  clearData     = flip setData ""
  getState      = const $ return (True, True)
  resetModified = const $ return ()
  focusView     = widgetGrabFocus . urlEntry

makeURLEntry _ _ = do
  box   <- hBoxNew False 0
  containerSetBorderWidth box 7
  entry <- entryNew
  entrySetActivatesDefault entry True
  boxPackStartDefaults box entry
  return URLEntry { urlEntry = entry
                  , urlBox   = box
                  }

makeURLEntryDialog =
  makeEditorDialog [] makeURLEntry $ \v -> do
    let outerw = outer v
    windowSetTitle outerw "Add media"
    windowSetDefaultSize outerw 500 (-1)

runURLEntryDialog dlg = do
  liftIO $ runEditorDialog dlg
    (return "")
    (\str ->
      insertURIs (map (encString False ok_url) $ lines str) Nothing)
    False window
