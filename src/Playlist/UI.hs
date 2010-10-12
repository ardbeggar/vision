-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

module Playlist.UI
  ( setupUI
  , setupUIB
  ) where

import System.IO.Unsafe

import Network.URL

import Graphics.UI.Gtk hiding (add, remove)

import XMMS2.Client hiding (Data)

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
import Properties hiding (showPropertyEditor, showPropertyExport)
import Playlist.Model
import Playlist.View
import Playlist.Edit
import Playlist.Config
import Playlist.Control


setupUI = do
  addUIActions uiActions

  orderDialog <- unsafeInterleaveIO $ makeOrderDialog $ \v -> do
    let outerw = outer v
        updateTitle = do
          name <- getPlaylistName
          windowSetTitle outerw $
            "Sort playlist" ++ maybe "" (": " ++) name
    cid <- onPlaylistUpdated . add . ever . const $ updateTitle
    outerw `onDestroy` (onPlaylistUpdated $ remove cid)
    updateTitle
    windowSetDefaultSize outerw 500 400

  urlEntryDialog <- unsafeInterleaveIO $ makeURLEntryDialog

  srvAG <- actionGroupNew "server"
  actionGroupAddActions srvAG $ srvActions orderDialog urlEntryDialog
  onServerConnectionAdd . ever $ actionGroupSetSensitive srvAG
  insertActionGroup srvAG 1

  play  <- getAction srvAG "play"
  pause <- getAction srvAG "pause"
  stop  <- getAction srvAG "stop"
  let setupPPS = do
        ps <- getPlaybackStatus
        let (ePlay, ePause, eStop) = case ps of
              Just StatusPlay  -> (False, True, True)
              Just StatusPause -> (True, False, True)
              _                -> (True, False, False)
        actionSetSensitive play ePlay
        actionSetVisible play ePlay
        actionSetSensitive pause ePause
        actionSetVisible pause ePause
        actionSetSensitive stop eStop

  prev <- getAction srvAG "prev"
  next <- getAction srvAG "next"
  let setupPN = do
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

  cut    <- getAction srvAG "cut"
  copy   <- getAction srvAG "copy"
  delete <- getAction srvAG "delete"
  editp  <- getAction srvAG "edit-properties"
  expp   <- getAction srvAG "export-properties"
  let setupSel = do
        n <- treeSelectionCountSelectedRows playlistSel
        mapM_ (`actionSetSensitive` (n /= 0))
          [cut, copy, delete, editp, expp]

  paste  <- getAction srvAG "paste"
  append <- getAction srvAG "append"
  let setupPA = do
        en <- editCheckClipboard
        mapM_ (`actionSetSensitive` en) [paste, append]

  onPlaybackStatus   . add . ever . const $ setupPPS
  onCurrentTrack     . add . ever . const $ setupPN
  onPlaylistUpdated  . add . ever . const $ setupPN
  onClipboardTargets . add . ever . const $ setupPA
  playlistSel `onSelectionChanged` setupSel
  flip timeoutAdd 0 $ do
    setupPPS
    setupPN
    setupSel
    setupPA
    return False

  addUIFromFile "playlist"

  playlistView `onRowActivated` \[n] _ ->
    playTrack n

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll playlistView
  boxPackStartDefaults contents scroll

  playbar <- getWidget castToToolbar "ui/playbar"
  toolbarSetStyle playbar ToolbarIcons
  boxPackEnd contents playbar PackNatural 0

  sep <- separatorToolItemNew
  separatorToolItemSetDraw sep False
  toolbarInsert playbar sep (-1)

  seekView <- makeSeekControl
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

  popup <- getWidget castToMenu "ui/playlist-popup"
  setupTreeViewPopup playlistView popup

  window `onDestroy` mainQuit

  onPlaylistUpdated . add . ever . const $ updateWindowTitle
  updateWindowTitle


uiActions =
  [ ActionEntry
    { actionEntryName        = "properties"
    , actionEntryLabel       = "P_roperties"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "manage-properties"
    , actionEntryLabel       = "_Manage properties"
    , actionEntryStockId     = Just stockPreferences
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showPropertyManager
    }
  , ActionEntry
    { actionEntryName        = "playlist-popup"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  ]

srvActions _orderDialog _urlEntryDialog = []

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

updateWindowTitle = do
  maybeName <- getPlaylistName
  setWindowTitle $ case maybeName of
    Nothing   -> "Vision playlist"
    Just name -> name ++ " - Vision playlist"

setupUIB builder = do
  setupUIActions builder
  setupServerActions builder
  setupPlaybar builder

setupPlaybar builder = do
  playbar <- builderGetObject builder castToToolbar "playbar"

  sep <- separatorToolItemNew
  separatorToolItemSetDraw sep False
  toolbarInsert playbar sep (-1)

  seekView <- makeSeekControl
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

setupServerActions builder = do
  urlEntryDialog <- unsafeInterleaveIO $ makeURLEntryDialog

  orderDialog <- unsafeInterleaveIO $ makeOrderDialog $ \v -> do
    let outerw = outer v
        updateTitle = do
          name <- getPlaylistName
          windowSetTitle outerw $
            "Sort playlist" ++ maybe "" (": " ++) name
    cid <- onPlaylistUpdated . add . ever . const $ updateTitle
    outerw `onDestroy` (onPlaylistUpdated $ remove cid)
    updateTitle
    windowSetDefaultSize outerw 500 400

  ag <- builderGetObject builder castToActionGroup "server-actions"
  onServerConnectionAdd . ever $ actionGroupSetSensitive ag

  play   <- action builder "play"              $ startPlayback False
  pause  <- action builder "pause"             $ pausePlayback
  stop   <- action builder "stop"              $ stopPlayback
  prev   <- action builder "prev"              $ prevTrack
  next   <- action builder "next"              $ nextTrack
  cut    <- action builder "cut"               $ editDelete True
  copy   <- action builder "copy"              $ editCopy
  paste  <- action builder "paste"             $ editPaste False
  append <- action builder "append"            $ editPaste True
  delete <- action builder "delete"            $ editDelete False
  editp  <- action builder "edit-properties"   $ showPropertyEditor
  export <- action builder "export-properties" $ showPropertyExport

  action builder "select-all"        $ editSelectAll
  action builder "invert-selection"  $ editInvertSelection
  action builder "browse-location"   $ browseLocation SortAscending Nothing
  action builder "browse-collection" $ browseCollection Nothing
  action builder "add-media"         $ runURLEntryDialog urlEntryDialog
  action builder "clear-playlist"    $ clearPlaylist
  action builder "sort-by"           $ showOrderDialog orderDialog getOrder setOrder
  action builder "import-properties" $ showPropertyImport

  let setupPPS = do
        ps <- getPlaybackStatus
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
        mapM_ (`actionSetSensitive` en) [paste, append]

  onPlaybackStatus   . add . ever . const $ setupPPS
  onCurrentTrack     . add . ever . const $ setupPN
  onPlaylistUpdated  . add . ever . const $ (setupPN >> updateWindowTitle)
  onClipboardTargets . add . ever . const $ setupPA
  playlistSel `onSelectionChanged` setupSel
  flip timeoutAdd 0 $ do
    setupPPS
    setupPN
    setupPA
    setupSel
    updateWindowTitle
    return False

  return ()


setupUIActions builder = do
  action builder "quit"               mainQuit
  action builder "configure-playlist" showPlaylistConfigDialog
  action builder "manage-properties"  showPropertyManager
  return ()

action builder name func = do
  a <- builderGetObject builder castToAction name
  a `on` actionActivated $ func
  return a

