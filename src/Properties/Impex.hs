-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 12 Jul. 2010
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

{-# LANGUAGE DoRec, TupleSections, DeriveDataTypeable, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Properties.Impex
  ( initImpex
  , showPropertyExport
  , showPropertyImport
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Typeable
import Data.Env

import Codec.Binary.UTF8.String

import System.FilePath
import System.IO.Error
import System.IO.Unsafe

import Text.JSON

import Graphics.UI.Gtk

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client as X

import UI
import Registry
import Medialib
import Utils
import XMMS


data Ix = Ix deriving (Typeable)

data Impex
  = Impex { _export :: WithUI ([MediaId] -> IO ())
          , _import :: WithUI (IO ())
          }
    deriving (Typeable)


showPropertyImport = do
  Just (Env impex) <- getEnv (Extract :: Extract Ix Impex)
  _import impex

showPropertyExport ids = do
  Just (Env impex) <- getEnv (Extract :: Extract Ix Impex)
  _export impex ids

initImpex = do
  impex <- mkImpex
  addEnv Ix impex

mkImpex = do
  exportDlg <- unsafeInterleaveIO makeExportDlg
  importDlg <- unsafeInterleaveIO makeImportDlg
  return Impex { _export = \ids -> runChooser exportDlg (exportProps ids)
               , _import = runChooser importDlg importProps
               }

makeExportDlg =
  makeChooser "Export properties" FileChooserActionSave stockSave

exportProps ids file =
  retrieveProperties ids $ either (const $ return ()) $ \list -> do
    let base = dropFileName $ decodeString file
        text = encodeStrict $ showJSON $ map (exConv base . snd) list
    writeFile file text `catch` \e ->
      informUser MessageError $ "Property export failed: " ++
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

makeImportDlg =
  makeChooser "Import properties" FileChooserActionOpen stockOpen

importProps name =
  importAll `catch` (erep . ioeGetErrorString)
  where importAll = do
          text <- readFile name
          case decodeStrict text of
            Ok recs -> mapM_ (importOne base) recs
            Error _ -> erep "invalid file format"
        base = dropFileName decn
        decn = decodeString name
        erep = informUser MessageError . (("Property import failed: " ++ decn ++ ": ") ++)

importOne base ((url, args), props) =
  medialibAddEntryEncoded xmms enc >>* do
    liftIO $ medialibGetIdEncoded xmms enc >>* do
      id <- result
      unless (id == 0) $
        liftIO $ setProps id props
  where enc  = (encodeURL url') ++ args
        url' | isInfixOf "://" url = url
             | otherwise           = "file://" ++ joinPath [base, url]

setProps id props = mapM_ set $ Map.toList props
  where set (k, v) = medialibEntryPropertySet xmms id src k v
        src        = Just "client/generic/override/vision"



data Chooser
  = Chooser { cLock    :: MVar ()
            , cChooser :: FileChooserDialog
            }

makeChooser title action stockId = do
  lock    <- newMVar ()
  chooser <- fileChooserDialogNew
             (Just title)
             Nothing
             action
             [ (stockCancel, ResponseCancel)
             , (stockId,     ResponseAccept) ]
  hideOnDeleteEvent chooser

  filter <- fileFilterNew
  fileFilterSetName filter "Vision property files"
  fileFilterAddCustom filter [FileFilterFilename] $ \name _ _ _ ->
    return $ maybe False ((==) ".vpf" . map toLower . takeExtension) name
  fileChooserAddFilter chooser filter

  filter <- fileFilterNew
  fileFilterSetName filter "All files"
  fileFilterAddCustom filter [] $ \_ _ _ _ -> return True
  fileChooserAddFilter chooser filter

  return Chooser { cLock    = lock
                 , cChooser = chooser
                 }

runChooser Chooser { cLock = lock, cChooser = chooser } onAccept = do
  locked <- isJust <$> tryTakeMVar lock
  when locked $ do
    windowSetTransientFor chooser window
    rec { cid <- chooser `onResponse` \resp -> do
             signalDisconnect cid
             case resp of
               ResponseAccept -> do
                 name <- fileChooserGetFilename chooser
                 withJust name onAccept
               _ ->
                 return ()
             widgetHide chooser
             putMVar lock ()
        }
    return ()
  windowPresent chooser


instance JSON X.Property where
  showJSON (X.PropInt32 i)  = showJSON i
  showJSON (X.PropString s) = showJSON s
  readJSON (JSRational _ i) = return $ X.PropInt32  $ truncate i
  readJSON (JSString s)     = return $ X.PropString $ fromJSString s
