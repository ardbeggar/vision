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

import Control.Applicative
import Data.Maybe

import Graphics.UI.Gtk hiding (add)

import XMMS2.Client

import UI
import Environment
import XMMS
import Handler
import Playback
import Playlist.Model
import Playlist.View


setupUI = do
  uim <- uiManagerNew
  windowAddAccelGroup window =<< uiManagerGetAccelGroup uim

  uiAG <- actionGroupNew "ui"
  uiManagerInsertActionGroup uim uiAG 1
  actionGroupAddActions uiAG uiActions

  srvAG <- actionGroupNew "server"
  onConnected . add . ever . const $ actionGroupSetSensitive srvAG True
  onDisconnected . add . ever . const $ actionGroupSetSensitive srvAG False
  uiManagerInsertActionGroup uim srvAG 1

  let addA name text stockId accel func = do
        a <- actionNew name text Nothing stockId
        a `on` actionActivated $ (func >> return ())
        actionGroupAddActionWithAccel srvAG a accel
        return a
  play  <- addA "play" "_Play" (Just stockMediaPlay) (Just "<Control>space") startPlayback
  pause <- addA "pause" "_Pause" (Just stockMediaPause) (Just "<Control>space") pausePlayback
  stop  <- addA "stop" "_Stop" (Just stockMediaStop) (Just "<Control>s") stopPlayback
  prev  <- addA "prev" "P_revious track" (Just stockMediaPrevious) (Just "<Control>p") prevTrack
  next  <- addA "next" "_Next track" (Just stockMediaNext) (Just "<Control>n") nextTrack
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
  onPlaybackStatus . add . ever . const $ setupPPS
  onCurrentTrack . add . ever . const $ setupPN
  onPlaylistUpdated . add . ever . const $ setupPN
  flip timeoutAdd 0 $ do
    setupPPS
    setupPN
    return False

  uiManagerAddUiFromFile uim $ uiFilePath "playlist"

  box <- vBoxNew False 0
  containerAdd window box

  menubar <- castToMenuBar . fromJust <$> uiManagerGetWidget uim "ui/menubar"
  boxPackStart box menubar PackNatural 0

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  containerAdd scroll playlistView
  boxPackStartDefaults box scroll

  playbar <- castToToolbar . fromJust <$> uiManagerGetWidget uim "ui/playbar"
  toolbarSetStyle playbar ToolbarIcons
  boxPackEnd box playbar PackNatural 0

  window `onDestroy` mainQuit


uiActions =
  [ ActionEntry
    { actionEntryName        = "menubar"
    , actionEntryLabel       = ""
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
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
  ]
