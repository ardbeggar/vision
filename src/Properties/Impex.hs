-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 12 Jul. 2010
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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.Impex
  ( initImpex
  , showPropertyExport
  , showPropertyImport
  ) where

import Control.Monad
import Control.Monad.Trans

import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import Codec.Binary.UTF8.String

import System.FilePath
import System.IO.Error

import Text.JSON

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client as X

import CODW
import Context
import Medialib
import Utils
import XMMS


data Impex
  = Impex { iExportDlg :: CODW [MediaId] FileChooserDialog
          , iImportDlg :: CODW () FileChooserDialog
          }


showPropertyExport ids =
  showCODW ids $ iExportDlg context

showPropertyImport =
  showCODW () $ iImportDlg context


initImpex = do
  context <- initContext
  let ?context = context

  return ?context

initContext = do
  exportDlg <- makeCODW makeExportDlg
  importDlg <- makeCODW $ const makeImportDlg
  return $ augmentContext
    Impex { iExportDlg = exportDlg
          , iImportDlg = importDlg
          }

makeExportDlg ids = do
  chooser <- fileChooserDialogNew
             (Just "Export properties")
             Nothing
             FileChooserActionSave
             [ (stockCancel, ResponseCancel)
             , (stockOk,     ResponseAccept) ]
  addFilters chooser
  chooser `onResponse` \resp -> do
    case resp of
      ResponseAccept -> do
        name <- fileChooserGetFilename chooser
        fmaybeM_ name $ exportProps ids
      _ ->
        return ()
    widgetDestroy chooser

  return chooser

exportProps ids file = do
  pbar <- progressBarNew -- FIXME
  retrieveProperties pbar ids $ \list -> do
    let base = dropFileName $ decodeString file
        text = encodeStrict $ showJSON $ map (exConv base . snd) list
    widgetDestroy pbar -- FIXME
    writeFile file text `catch` \e ->
      putStrLn $ "Export failed" ++
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

makeImportDlg = do
  chooser <- fileChooserDialogNew
             (Just "Import properties")
             Nothing
             FileChooserActionOpen
             [ (stockCancel, ResponseCancel)
             , (stockOk,     ResponseAccept) ]
  addFilters chooser
  chooser `onResponse` \resp -> do
    case resp of
      ResponseAccept -> do
        name <- fileChooserGetFilename chooser
        fmaybeM_ name importProps
      _ ->
        return ()
    widgetDestroy chooser

  return chooser

importProps name =
  importAll `catch` (erep . ioeGetErrorString)
  where importAll = do
          text <- readFile name
          case decodeStrict text of
            Ok recs -> mapM_ (importOne base) recs
            Error _ -> erep "invalid file format"
        base = dropFileName decn
        decn = decodeString name
        erep = putStrLn . (("Import failed: " ++ decn ++ ": ") ++)

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


addFilters chooser = do
  vpfFilter <- fileFilterNew
  fileFilterSetName vpfFilter "Vision property files"
  fileFilterAddCustom vpfFilter [FileFilterFilename] $ \name _ _ _ ->
    return $ maybe False ((==) ".vpf" . map toLower . takeExtension) name
  fileChooserAddFilter chooser vpfFilter

  allFilter <- fileFilterNew
  fileFilterSetName allFilter "All files"
  fileFilterAddCustom allFilter [] $ \_ _ _ _ -> return True
  fileChooserAddFilter chooser allFilter


instance JSON X.Property where
  showJSON (X.PropInt32 i)  = showJSON i
  showJSON (X.PropString s) = showJSON s
  readJSON (JSRational _ i) = return $ X.PropInt32  $ truncate i
  readJSON (JSString s)     = return $ X.PropString $ fromJSString s

