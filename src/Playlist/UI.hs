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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import System.IO.Unsafe

import Network.URL

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client hiding (Data, playbackStatus)

import UI
import XMMS
import Handler
import Playback
import Playtime
import Volume
import Utils
import Clipboard
import Location
import Collection
import Compound
import Editor
import Context
import Properties hiding (showPropertyEditor, showPropertyExport)
import Playlist.Model
import Playlist.View
import Playlist.Edit
import Playlist.Config
import Playlist.Control


setupUI builder = do
  Just env <- getEnv playtimeEnv
  runEnvT env $ runIn playtimeEnv $>
    setupPlaybar builder

  Just env <- getEnv clipboardEnv
  runEnvT env $ runIn clipboardEnv $>
    setupActions builder

  liftIO $ do
    playlistView `onRowActivated` \[n] _ ->
      playTrack n

    popup <- getWidget castToMenu "ui/playlist-popup"
    setupTreeViewPopup playlistView popup

    window `onDestroy` mainQuit

  return ()


setupActions builder = do
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

  ag <- liftIO $ builderGetObject builder castToActionGroup "server-actions"
  liftIO $ onServerConnectionAdd . ever $ actionGroupSetSensitive ag

  withW (runIn clipboardEnv) $ \runC ->
    liftIO $ bindActions builder
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
    , ("browse-collection",  browseCollection Nothing)
    , ("add-media",          runURLEntryDialog urlEntryDialog)
    , ("clear-playlist",     clearPlaylist)
    , ("sort-by",            showOrderDialog orderDialog getOrder setOrder)
    , ("configure-playlist", showPlaylistConfigDialog)
    , ("edit-properties",    showPropertyEditor)
    , ("export-properties",  showPropertyExport)
    , ("import-properties",  showPropertyImport)
    , ("manage-properties",  showPropertyManager)
    ]

  play   <- liftIO $ action builder "play"
  pause  <- liftIO $ action builder "pause"
  stop   <- liftIO $ action builder "stop"
  prev   <- liftIO $ action builder "prev"
  next   <- liftIO $ action builder "next"
  cut    <- liftIO $ action builder "cut"
  copy   <- liftIO $ action builder "copy"
  paste  <- liftIO $ action builder "paste"
  append <- liftIO $ action builder "append"
  delete <- liftIO $ action builder "delete"
  editp  <- liftIO $ action builder "edit-properties"
  export <- liftIO $ action builder "export-properties"

  let setupPPS ps = do
        let (ePlay, ePause, eStop) = case ps of
              Just StatusPlay  -> (False, True, True)
              Just StatusPause -> (True, False, True)
              _                -> (True, False, False)
        actionSetSensitive play ePlay
        actionSetVisible play ePlay
        actionSetSensitive pause ePause
        actionSetVisible pause ePause
        actionSetSensitive stop eStop
      setupPN = do
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
      setupSel = do
        n <- treeSelectionCountSelectedRows playlistSel
        mapM_ (`actionSetSensitive` (n /= 0))
          [cut, copy, delete, editp, export]
      setupPA = do
        en <- editCheckClipboard
        liftIO $ mapM_ (`actionSetSensitive` en) [paste, append]

  ct <- clipboardTargets
  runIn clipboardEnv $> do
    io $ \run -> do
      ctW <- atomically $ newEmptyTWatch ct
      forkIO $ forever $ do
        void $ atomically $ watch ctW
        run setupPA

  liftIO $ do
    psW <- atomically $ newEmptyTWatch playbackStatus
    forkIO $ forever $ do
      ps <- atomically $ watch psW
      setupPPS ps

    ctW <- atomically $ newEmptyTWatch currentTrack
    psW <- atomically $ newEmptyTWatch playlistSize
    forkIO $ forever $ do
      atomically $ (void $ watch ctW) `mplus` (void $ watch psW)
      setupPN

    pnW <- atomically $ newEmptyTWatch playlistName
    forkIO $ forever $ do
      name <- atomically $ watch pnW
      setWindowTitle $ maybe "Vision playlist" (++ " - Vision playlist") name

    playlistSel `onSelectionChanged` setupSel
    postGUIAsync setupSel

  return ()


setupPlaybar builder = do
  seekView <- makeSeekControl

  liftIO $ do
    playbar <- builderGetObject builder castToToolbar "playbar"

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

    volView <- makeVolumeControl
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

runURLEntryDialog dlg =
  runEditorDialog dlg (return "")
  (\str ->
    insertURIs (map (encString False ok_url) $ lines str) Nothing)
  False window
