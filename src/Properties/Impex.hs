-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 31 Oct. 2009
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module Properties.Impex
  ( setupImpex
  ) where

import Prelude hiding (catch)

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent

import Data.IORef
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map

import Text.JSON

import Codec.Binary.UTF8.String

import System.FilePath
import System.IO.Error

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client.Types as X

import Utils
import UI
import XMMS
import Medialib

import Properties.Model

import Debug.Trace


setupImpex = do
  context <- trace "    init context" initContext
  let ?context = context
  trace "    setup import" setupImport
  trace "    setup export" setupExport
  trace "    all done" $ return ()


setupImport = do
  chooser <- trace "      create chooser" $
             fileChooserDialogNew
             (Just "Import properties")
             (Just mainWindow)
             FileChooserActionOpen
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-ok", ResponseAccept) ]
  trace "      fix chooser" $ fixWindow chooser
  trace "      add vpf filter" $ fileChooserAddFilter chooser vpfFilter
  trace "      add all filter" $ fileChooserAddFilter chooser allFilter
  chooser `onResponse` \resp -> do
    widgetHide chooser
    case resp of
      ResponseAccept -> do
        name <- fileChooserGetFilename chooser
        maybe (return ()) importProps name
      _ ->
        return ()

  trace "      add action" $
    addAction "import-properties" "_Import properties…" stockOpen $ do
      windowPresent chooser

  trace "      all done" $ return ()


importProps name =
  importAll `catch` (erep . ioeGetErrorString)
  where importAll = do
          text <- readFile name
          case decodeStrict text of
            Ok recs -> mapM_ (importOne base) recs
            Error _ -> erep "invalid file format"
        base = dropFileName decn
        decn = decodeString name
        erep = reportError "Import failed" . ((decn ++ ": ") ++)

importOne base ((url, args), props) =
  medialibAddEntryEncoded xmms enc >>* do
    liftIO $ medialibGetIdEncoded xmms enc >>* do
      id <- result
      unless (id == 0) $
        liftIO $ setProps id props
      return False
    return False
  where enc  = (encodeURL url') ++ args
        url' | isInfixOf "://" url = url
             | otherwise           = "file://" ++ joinPath [base, url]

setProps id props = mapM_ set $ Map.toList props
  where set (k, v) = medialibEntryPropertySet xmms id src k v
        src        = Just "client/generic/override/vision"


setupExport = do
  lock <- newMVar ()
  let tryLock f = maybe (return ()) (const f) =<< tryTakeMVar lock
      unlock    = putMVar lock ()

  propsRef <- newIORef []

  chooser <- fileChooserDialogNew
             (Just "Export properties")
             (Just mainWindow)
             FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-ok", ResponseAccept) ]
  fixWindow chooser
  fileChooserAddFilter chooser vpfFilter
  fileChooserAddFilter chooser allFilter
  chooser `onResponse` \resp -> do
    case resp of
      ResponseAccept -> do
        name <- fileChooserGetFilename chooser
        maybe (return ()) (exportProps propsRef) name
      _ ->
        return ()
    unlock
    widgetHide chooser

  exp <- addAction "export-properties" "E_xport properties…" stockSave $ do
    tryLock $ do
      ids <- getSelectedIds
      mapM getInfo (nub ids) >>= writeIORef propsRef . catMaybes
    windowPresent chooser

  actionSetSensitive exp False
  onIdsSelected $ actionSetSensitive exp . not . null

exportProps propsRef file = do
  props <- readIORef propsRef
  let base = dropFileName $ decodeString file
      text = encodeStrict $ showJSON $ map (exConv base) props
  writeFile file text `catch` \e ->
    reportError "Export failed" $
      (decodeString file) ++ ": " ++ ioeGetErrorString e

exConv base info = ((url', args), Map.difference info readOnlyProps)
  where url'             = stripBase $ decodeURL path
        (path, args)     = break (== '?') url
        X.PropString url = fromJust $ Map.lookup "url" info
        stripBase url
          | Just path <- stripPrefix "file://" url
          , Just tail <- stripPrefix base path = tail
          | otherwise = url

readOnlyProps =
  Map.fromList $ map (, undefined)
  [ "added", "bitrate", "chain", "channels", "duration"
  , "id", "laststarted", "lmod", "mime", "sample_format"
  , "samplerate", "size", "status", "timesplayed", "url"
  , "startms", "stopms", "isdir", "intsort" ]


data Context
  = Context { eActionGroup :: ActionGroup
        , eVpfFilter   :: FileFilter
        , eAllFilter   :: FileFilter }

addAction name text stockId onActivated = do
  action <- actionNew name text Nothing (Just stockId)
  actionGroupAddAction (eActionGroup ?context) action
  action `on` actionActivated $ onActivated
  return action

vpfFilter = eVpfFilter ?context
allFilter = eAllFilter ?context

initContext = do
  actionGroup <- actionGroupNew "property-impex"
  insertActionGroup actionGroup 1
  actionGroupSetSensitive actionGroup False
  onConnected    $ actionGroupSetSensitive actionGroup True
  onDisconnected $ actionGroupSetSensitive actionGroup False

  vpfFilter <- fileFilterNew
  fileFilterSetName vpfFilter "Vision property files"
  fileFilterAddCustom vpfFilter [FileFilterFilename] $ \name _ _ _ ->
    return $ maybe False ((==) ".vpf" . map toLower . takeExtension) name

  allFilter <- fileFilterNew
  fileFilterSetName allFilter "All files"
  fileFilterAddCustom allFilter [] $ \_ _ _ _ -> return True

  return Context { eActionGroup = actionGroup
             , eVpfFilter   = vpfFilter
             , eAllFilter   = allFilter }


instance JSON X.Property where
  showJSON (X.PropInt32 i)  = showJSON i
  showJSON (X.PropString s) = showJSON s
  readJSON (JSRational _ i) = return $ X.PropInt32  $ truncate i
  readJSON (JSString s)     = return $ X.PropString $ fromJSString s




