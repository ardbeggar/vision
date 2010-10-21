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

module Location.Control
  ( loadLocation
  , makeURL
  , addToPlaylist
  , replacePlaylist
  , updateWindowTitle
  , openLocation
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO
import Control.Monad.Trans

import Data.Maybe
import Data.List

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Environment
import UI
import Location.History
import Location.Model
import Location.View
import Location.PathComp


loadLocation location = do
  listStoreClear locationStore
  maybeURL <- updateLocation location
  case maybeURL of
    Just url -> do
      addToHistory url
      updateWindowTitle
      clearPathComp locationComp
      entrySetText locationEntry url
      xformMediaBrowse xmms url >>* handleBrowse url
    Nothing ->
      return ()

handleBrowse url =
  handleBrowse' `catch` \(_ :: XMMSException) ->
    liftIO $ putStrLn $ "error loading " ++ url
  where handleBrowse' = do
          r <- result
          liftIO $ do
            mapM_ (listStoreAppend locationStore . makeItem) r
            widgetGrabFocus locationView

makeURL url
  | "://" `isInfixOf` url =
    url
  | "~/" `isPrefixOf` url && isJust homeDir =
    "file://" ++ fromJust homeDir ++ tail url
  | otherwise =
    "file://" ++ url

replacePlaylist = do
  playlistClear xmms Nothing
  addToPlaylist

addToPlaylist = do
  rows <- treeSelectionGetSelectedRows locationSel
  mapM_ addOne rows

addOne p = do
  item <- itemByPath p
  let path = iPath item
  if iIsDir item
    then do
      playlistRAdd xmms Nothing path
      return ()
    else do
      collIdlistFromPlaylistFile xmms path >>*
        ((do coll <- result
             liftIO $ playlistAddIdlist xmms Nothing coll
             return ())
         `catch`
         (\(_ :: XMMSException) -> do
             liftIO $ playlistAddURL xmms Nothing path
             return ()))
      return ()

updateWindowTitle = do
  loc <- getCurrentLocation
  setWindowTitle $ case loc of
    [] -> "Vision location browser"
    _  -> loc ++ " - Vision location browser"

openLocation = do
  widgetGrabFocus locationEntry
  editableSelectRegion locationEntry 0 (-1)
