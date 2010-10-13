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

module Location.UI
  ( setupUI
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk hiding (add)

import UI
import XMMS
import Handler
import Utils
import Location.Model
import Location.View
import Location.Control


setupUI builder browse = do
  setupActions builder browse
  setupToolbar builder
  setupLocationEntry builder
  setupLocationView builder

setupActions builder browse = do
  ag <- builderGetObject builder castToActionGroup "server-actions"
  onServerConnectionAdd . ever $ actionGroupSetSensitive ag

  bindActions builder
    [ ("new-window"           , newWindow browse                )
    , ("open-location"        , openLocation                    )
    , ("load"                 , loadCurrentLocation             )
    , ("down"                 , loadAtCursor (loadLocation . Go))
    , ("browse-in-new-window" , browseInNewWindow browse        )
    , ("add-to-playlist"      , addToPlaylist                   )
    , ("replace-playlist"     , replacePlaylist                 )
    , ("back"                 , loadLocation Back               )
    , ("forward"              , loadLocation Forward            )
    , ("up"                   , loadLocation Up                 )
    , ("refresh"              , loadLocation Refresh            )
    ]

  down    <- action builder "down"
  binw    <- action builder "browse-in-new-window"
  addp    <- action builder "add-to-playlist"
  repp    <- action builder "replace-playlist"
  back    <- action builder "back"
  forward <- action builder "forward"
  up      <- action builder "up"
  refresh <- action builder "refresh"

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

  locationSel `onSelectionChanged` updateB
  onLocation . add . ever . const $ updateN
  flip timeoutAdd 0 $ do
    updateB
    updateN
    updateWindowTitle
    return False

  return ()

setupToolbar builder = do
  toolbar <- builderGetObject builder castToToolbar "toolbar"

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

setupLocationEntry builder = do
  load <- action builder "load"
  locationEntry `onEntryActivate` actionActivate load
  locationEntry `onIconPress` \pos ->
    case pos of
      0 -> entrySetText locationEntry ""
      1 -> actionActivate load
      _ -> return ()

  return ()

setupLocationView builder = do
  popup <- getWidget castToMenu "ui/location-popup"
  setupTreeViewPopup locationView popup

  down <- action builder "down"
  locationView `onRowActivated` \_ _ ->
    actionActivate down

  binw <- action builder "browse-in-new-window"
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
