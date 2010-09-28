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

import System.IO.Unsafe

import Graphics.UI.Gtk hiding (add)

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
    windowSetTitle outerw "Sort playlist"
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
    { actionEntryName        = "edit"
    , actionEntryLabel       = "_Edit"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "browse"
    , actionEntryLabel       = "_Browse"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "playlist"
    , actionEntryLabel       = "_Playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = return ()
    }
  , ActionEntry
    { actionEntryName        = "configure-playlist"
    , actionEntryLabel       = "C_onfigure playlist"
    , actionEntryStockId     = Just stockPreferences
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showPlaylistConfigDialog
    }
  , ActionEntry
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

srvActions orderDialog urlEntryDialog =
  [ ActionEntry
    { actionEntryName        = "play"
    , actionEntryLabel       = "_Play"
    , actionEntryStockId     = Just stockMediaPlay
    , actionEntryAccelerator = Just "<Control>space"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = startPlayback False
    }
  , ActionEntry
    { actionEntryName        = "pause"
    , actionEntryLabel       = "_Pause"
    , actionEntryStockId     = Just stockMediaPause
    , actionEntryAccelerator = Just "<Control>space"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = pausePlayback
    }
  , ActionEntry
    { actionEntryName        = "stop"
    , actionEntryLabel       = "_Stop"
    , actionEntryStockId     = Just stockMediaStop
    , actionEntryAccelerator = Just "<Control>s"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = stopPlayback
    }
  , ActionEntry
    { actionEntryName        = "prev"
    , actionEntryLabel       = "P_revious track"
    , actionEntryStockId     = Just stockMediaPrevious
    , actionEntryAccelerator = Just "<Control>p"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = prevTrack
    }
  , ActionEntry
    { actionEntryName        = "next"
    , actionEntryLabel       = "_Next track"
    , actionEntryStockId     = Just stockMediaNext
    , actionEntryAccelerator = Just "<Control>n"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = nextTrack
    }
  , ActionEntry
    { actionEntryName        = "cut"
    , actionEntryLabel       = "Cu_t"
    , actionEntryStockId     = Just stockCut
    , actionEntryAccelerator = Just "<Control>x"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editDelete True
    }
  , ActionEntry
    { actionEntryName        = "copy"
    , actionEntryLabel       = "_Copy"
    , actionEntryStockId     = Just stockCopy
    , actionEntryAccelerator = Just "<Control>c"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editCopy
    }
  , ActionEntry
    { actionEntryName        = "paste"
    , actionEntryLabel       = "_Paste"
    , actionEntryStockId     = Just stockPaste
    , actionEntryAccelerator = Just "<Control>v"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editPaste False
    }
  , ActionEntry
    { actionEntryName        = "append"
    , actionEntryLabel       = "_Append"
    , actionEntryStockId     = Just stockPaste
    , actionEntryAccelerator = Just "<Control><Shift>v"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editPaste True
    }
  , ActionEntry
    { actionEntryName        = "delete"
    , actionEntryLabel       = "_Delete"
    , actionEntryStockId     = Just stockDelete
    , actionEntryAccelerator = Just "Delete"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editDelete False
    }
  , ActionEntry
    { actionEntryName        = "select-all"
    , actionEntryLabel       = "_Select all"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editSelectAll
    }
  , ActionEntry
    { actionEntryName        = "invert-selection"
    , actionEntryLabel       = "_Invert selection"
    , actionEntryStockId     = Just stockSelectAll
    , actionEntryAccelerator = Just "<Control><Shift>a"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = editInvertSelection
    }
  , ActionEntry
    { actionEntryName        = "browse-location"
    , actionEntryLabel       = "Browse _location"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = browseLocation SortAscending Nothing
    }
  , ActionEntry
    { actionEntryName        = "browse-collection"
    , actionEntryLabel       = "Browse _collection"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = browseCollection Nothing
    }
  , ActionEntry
    { actionEntryName        = "add-media"
    , actionEntryLabel       = "_Add media"
    , actionEntryStockId     = Just stockAdd
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = runURLEntryDialog urlEntryDialog
    }
  , ActionEntry
    { actionEntryName        = "clear-playlist"
    , actionEntryLabel       = "_Clear playlist"
    , actionEntryStockId     = Just stockClear
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = clearPlaylist
    }
  , ActionEntry
    { actionEntryName        = "sort-playlist"
    , actionEntryLabel       = "_Sort playlist"
    , actionEntryStockId     = Nothing
    , actionEntryAccelerator = Nothing
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showOrderDialog orderDialog getOrder setOrder
    }
  , ActionEntry
    { actionEntryName        = "edit-properties"
    , actionEntryLabel       = "_Edit properties"
    , actionEntryStockId     = Just stockEdit
    , actionEntryAccelerator = Just "<Alt>Return"
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showPropertyEditor
    }
  , ActionEntry
    { actionEntryName        = "export-properties"
    , actionEntryLabel       = "E_xport properties"
    , actionEntryStockId     = Just stockSave
    , actionEntryAccelerator = Just ""
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showPropertyExport
    }
  , ActionEntry
    { actionEntryName        = "import-properties"
    , actionEntryLabel       = "_Import properties"
    , actionEntryStockId     = Just stockOpen
    , actionEntryAccelerator = Just ""
    , actionEntryTooltip     = Nothing
    , actionEntryCallback    = showPropertyImport
    }
  ]


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
  entry <- entryNew
  box   <- hBoxNew False 0
  containerSetBorderWidth box 7
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
  runEditorDialog dlg (return "") (\uri -> insertURIs [uri] Nothing) False window