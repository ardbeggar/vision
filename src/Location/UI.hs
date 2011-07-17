-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 28 Jun. 2010
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

module Location.UI
  ( setupUI
  ) where

import Control.Applicative

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TGVar

import Graphics.UI.Gtk hiding (add)

import UI
import Builder
import XMMS
import Utils
import Location.Model
import Location.View
import Location.Control


setupUI browse = do
  setupActions browse
  setupToolbar
  setupLocationEntry
  setupLocationView
  setupConnection

setupActions browse = do
  bindActions
    [ ("new-window"           , newWindow browse                        )
    , ("open-location"        , openLocation                            )
    , ("load"                 , loadCurrentLocation               )
    , ("down"                 , loadAtCursor (loadLocation . Go))
    , ("browse-in-new-window" , browseInNewWindow browse                )
    , ("add-to-playlist"      , addToPlaylist                           )
    , ("replace-playlist"     , replacePlaylist                         )
    , ("back"                 , loadLocation Back               )
    , ("forward"              , loadLocation Forward            )
    , ("up"                   , loadLocation Up                 )
    , ("refresh"              , loadLocation Refresh            )
    ]

  down    <- action "down"
  binw    <- action "browse-in-new-window"
  addp    <- action "add-to-playlist"
  repp    <- action "replace-playlist"
  back    <- action "back"
  forward <- action "forward"
  up      <- action "up"
  refresh <- action "refresh"

  let updateB = do
        rows       <- treeSelectionGetSelectedRows locationSel
        (enp, enn) <- case rows of
          []    ->
            return (False, False)
          [path] -> do
            item <- itemByPath path
            return (True, iIsDir item)
          _     ->
            return (True, False)
        mapM_ (`actionSetSensitive` enp) [addp, repp]
        mapM_ (`actionSetSensitive` enn) [down, binw]
      updateN = do
        (eb, ef, eu, er) <- canGo
        actionSetSensitive back eb
        actionSetSensitive forward ef
        actionSetSensitive up eu
        actionSetSensitive refresh er

  lW <- atomically $ newTGWatch location
  forkIO $ forever $ do
    void $ atomically $ watch lW
    postGUISync $ do
      updateN
      updateWindowTitle

  locationSel `onSelectionChanged` updateB
  postGUIAsync updateB

  return ()

setupToolbar = do
  toolbar <- getObject castToToolbar "toolbar"

  item <- separatorToolItemNew
  separatorToolItemSetDraw item False
  toolbarInsert toolbar item 4

  item <- toolItemNew
  toolItemSetHomogeneous item False
  toolItemSetExpand item True
  containerAdd item locationEntry
  toolbarInsert toolbar item 5

  item <- separatorToolItemNew
  separatorToolItemSetDraw item False
  toolbarInsert toolbar item 6

setupLocationEntry = do
  load <- action "load"
  locationEntry `onEntryActivate` actionActivate load
  locationEntry `onIconPress` \icon ->
    case icon of
      PrimaryIcon   -> entrySetText locationEntry ""
      SecondaryIcon -> actionActivate load
  return ()

setupLocationView = do
  popup <- getWidget castToMenu "ui/location-popup"
  setupTreeViewPopup locationView popup

  down  <- action "down"
  locationView `onRowActivated` \_ _ ->
    actionActivate down

  binw  <- action "browse-in-new-window"
  locationView `on` buttonPressEvent $ tryEvent $ do
    MiddleButton <- eventButton
    SingleClick  <- eventClick
    (x, y)       <- eventCoordinates
    liftIO $ do
      maybePath <- treeViewGetPathAtPos locationView (round x, round y)
      case maybePath of
        Just (path, _, _) -> do
          treeViewSetCursor locationView path Nothing
          actionActivate binw
        Nothing           ->
          return ()

  return ()

setupConnection = do
  ag <- getObject castToActionGroup "server-actions"
  xcW <- atomically $ newTGWatch connectedV
  forkIO $ forever $ do
    conn <- atomically $ watch xcW
    actionGroupSetSensitive ag conn
    locationEntry `set` [secondaryIconSensitive := conn]

loadCurrentLocation = do
  text <- trim <$> entryGetText locationEntry
  case text of
    [] -> do
      cur <- getCurrentLocation
      case cur of
        [] -> return ()
        _  -> do
          entrySetText locationEntry cur
          widgetGrabFocus locationView
    _  -> loadLocation . Go $ makeURL text

loadAtCursor func = do
  (path, _) <- treeViewGetCursor locationView
  case path of
    [_] -> do
      item <- itemByPath path
      when (iIsDir item) $ func $ iPath item
    _   ->
      return ()

browseInNewWindow browse = do
  order <- getSortOrder
  loadAtCursor (browse order . Just)

newWindow browse = do
  order <- getSortOrder
  browse order Nothing

updateWindowTitle = do
  loc <- getCurrentLocation
  setWindowTitle $ case loc of
    [] -> "Vision location browser"
    _  -> loc ++ " - Vision location browser"
