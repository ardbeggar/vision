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
  ) where

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import UI
import XMMS
import Handler
import Playback
import Playtime
import Volume
import Playlist.Model
import Playlist.View
import Playlist.Edit


setupUI = do
  addUIActions uiActions

  srvAG <- actionGroupNew "server"
  onServerConnectionAdd . ever $ actionGroupSetSensitive srvAG
  insertActionGroup srvAG 1

  let addA name text stockId accel func = do
        a <- actionNew name text Nothing stockId
        a `on` actionActivated $ (func >> return ())
        actionGroupAddActionWithAccel srvAG a accel
        return a
  play  <- addA "play" "_Play" (Just stockMediaPlay) (Just "<Control>space") (startPlayback False)
  pause <- addA "pause" "_Pause" (Just stockMediaPause) (Just "<Control>space") pausePlayback
  stop  <- addA "stop" "_Stop" (Just stockMediaStop) (Just "<Control>s") stopPlayback
  prev  <- addA "prev" "P_revious track" (Just stockMediaPrevious) (Just "<Control>p") prevTrack
  next  <- addA "next" "_Next track" (Just stockMediaNext) (Just "<Control>n") nextTrack
  cut    <- addA "cut" "Cu_t" (Just stockCut) (Just "<Control>x") $ editDelete True
  copy   <- addA "copy" "_Copy" (Just stockCopy) (Just "<Control>c") editCopy
  _paste  <- addA "paste" "_Paste" (Just stockPaste) (Just "<Control>v") $ editPaste False
  _append <- addA "append" "_Append" (Just stockPaste) (Just "<Control><Shift>v") $ editPaste True
  delete <- addA "delete" "_Delete" (Just stockDelete) (Just "Delete") $ editDelete False
  addA "select-all" "_Select all" (Just stockSelectAll) (Just "<Control>a") editSelectAll
  addA "invert-selection" "_Invert selection" (Just stockSelectAll) (Just "<Control><Shift>a") editInvertSelection
  let setupPPS = do
        ps <- getPlaybackStatus
        case ps of
          Just StatusPlay -> do
            actionSetSensitive play False
            actionSetVisible play False
            actionSetSensitive pause True
            actionSetVisible pause True
            actionSetSensitive stop True
          Just StatusPause -> do
            actionSetSensitive play True
            actionSetVisible play True
            actionSetSensitive pause False
            actionSetVisible pause False
            actionSetSensitive stop True
          _ -> do
            actionSetSensitive play True
            actionSetVisible play True
            actionSetSensitive pause False
            actionSetVisible pause False
            actionSetSensitive stop False
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
        mapM_ (flip actionSetSensitive (n /= 0)) [cut, copy, delete]
  onPlaybackStatus . add . ever . const $ setupPPS
  onCurrentTrack . add . ever . const $ setupPN
  onPlaylistUpdated . add . ever . const $ setupPN
  playlistSel `onSelectionChanged` setupSel
  flip timeoutAdd 0 $ do
    setupPPS
    setupPN
    setupSel
    return False

  addUIFromFile "playlist"

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
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

  window `onDestroy` mainQuit


uiActions =
  [ ActionEntry
    { actionEntryName        = "music"
    , actionEntryLabel       = "_Music"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "quit"
    , actionEntryLabel       = "_Quit"
    , actionEntryStockId     = Just stockQuit
    , actionEntryAccelerator = Just "<Control>q"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = mainQuit
    }
  , ActionEntry
    { actionEntryName        = "edit"
    , actionEntryLabel       = "_Edit"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  ]