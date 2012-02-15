-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Feb. 2010
--
--  Copyright (C) 2009-2011 Oleg Belozeorov
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

{-# LANGUAGE TupleSections, DeriveDataTypeable, Rank2Types #-}

module Playlist.Format
  ( initFormat
  , withFormat
  , makeTrackInfo
  , getFormatDefs
  , putFormatDefs
  , TrackInfo (..)
  , trackInfoAttrs
  , trackInfoText
  , trackInfoDuration
  , formatsGeneration
  ) where

import Prelude hiding (lookup, catch)
import Control.Exception

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import Control.Applicative
import Control.Monad

import Data.IORef
import Data.Maybe
import Data.Either
import Data.Char (toLower)
import Data.Env
import Data.Typeable

import Graphics.UI.Gtk hiding (add)

import Medialib
import Properties
import Config
import Utils
import Registry
import Playlist.Format.Format
import Playlist.Format.Parser


data TrackInfo
  = TrackInfo { tAttrs    :: [AttrOp CellRendererText]
              , tText     :: String
              , tDuration :: String }

trackInfoAttrs Nothing  = [ cellTextMarkup := Just "" ]
trackInfoAttrs (Just i) = tAttrs i

trackInfoText Nothing  = ""
trackInfoText (Just i) = tText i

trackInfoDuration Nothing  = ""
trackInfoDuration (Just i) = tDuration i


data Ix = Ix deriving (Typeable)

data Format
  = Format { _makeInfoRef      :: IORef (MediaInfo -> IO ([AttrOp CellRendererText], String))
           , _formatDefsRef    :: IORef [String]
           , _lookupDuration   :: MediaInfo -> String
           , _lookupURL        :: MediaInfo -> String
           , _generation       :: TVar Integer
           }
    deriving (Typeable)

getMakeInfo = readIORef (_makeInfoRef ?_Playlist_Format)
putMakeInfo = writeIORef (_makeInfoRef ?_Playlist_Format)

getFormatDefs  = readIORef (_formatDefsRef ?_Playlist_Format)
putFormatDefs' = writeIORef (_formatDefsRef ?_Playlist_Format)
putFormatDefs defs = do
  putFormatDefs' defs
  saveFormatDefs
  updateFormats True

lookupDuration = _lookupDuration ?_Playlist_Format
lookupURL      = _lookupURL ?_Playlist_Format

formatsGeneration = _generation ?_Playlist_Format


initFormat = do
  format <- mkFormat
  addEnv Ix format
  let ?_Playlist_Format = format

  loadFormatDefs

  prW <- atomically $ newEmptyTWatch propertiesGeneration
  forkIO $ forever $ do
    void $ atomically $ watch prW
    postGUISync $ updateFormats True

  return ()

newtype Wrap a = Wrap { unWrap :: (?_Playlist_Format :: Format) => a }

withFormat    = withFormat' . Wrap
withFormat' w = do
  Just (Env format) <- getEnv (Extract :: Extract Ix Format)
  let ?_Playlist_Format = format in unWrap w


mkFormat = do
  formatDefsRef <- newIORef []
  makeInfoRef   <- newIORef $ const $ return ([], "")
  duration      <- fromJust <$> property "Duration"
  url           <- fromJust <$> property "URL"
  generation    <- newTVarIO 0
  return Format { _makeInfoRef    = makeInfoRef
                , _formatDefsRef  = formatDefsRef
                , _lookupDuration = maybe "" escapeMarkup . lookup duration
                , _lookupURL      = maybe "" escapeMarkup . lookup url
                , _generation     = generation
                }


loadFormatDefs = do
  putFormatDefs' . map trim =<< config "playlist-formats.conf" builtinFormats
  updateFormats False
  where builtinFormats =
          [ "<b>{Movement}[: {Title}]</b>\n\
            \{Composer} — {Work}[, {Catalog}]"
          , "<b>{Work}</b>[<b>, </b>{Catalog}]\n\
            \{Composer}"
          , "<b>{Movement}[: {Title}]</b>\n\
            \{Composer} — {Work}[, {Catalog}][\n\
            \{Performer}][\n\
            \[{Conductor}, ]{Orchestra}][\n\
            \[{Chorus master}, ]{Chorus}]"
          , "<b>{Work}</b>[<b>, </b>{Catalog}]\n\
            \{Composer}[\n\
            \{Performer}][\n\
            \[{Conductor}, ]{Orchestra}][\n\
            \[{Chorus master}, ]{Chorus}]"
          , "[<b>{Title}</b>\n]\
            \{Channel}"
          , "[{Track} ]<b>{Title}</b>\n\
            \{Artist} — {Album}" ]

saveFormatDefs = do
  writeConfig "playlist-formats.conf" =<< getFormatDefs
  return ()

getFormats = (rights . map parseFormat) <$> getFormatDefs

updateFormats notify = do
  putMakeInfo =<< makeMakeInfo =<< getFormats
  when notify $ atomically $ do
    g <- readTVar formatsGeneration
    writeTVar formatsGeneration $ g + 1

makeTrackInfo info = do
  makeInfo      <- getMakeInfo
  (attrs, text) <- makeInfo info
  return TrackInfo { tAttrs    = attrs
                   , tText     = text
                   , tDuration = lookupDuration info }

makeMakeInfo fs = do
  fs' <- rights <$> mapM cookFormat fs
  return $ \pm -> do
    let (text, ellipsize) =
          maybe (lookupURL pm, EllipsizeMiddle) (, EllipsizeEnd) $
          formatMediaInfo fs' pm
    search <- map toLower <$> plain text
    return ( [ cellTextMarkup    := Just text
             , cellTextEllipsize := ellipsize ]
           , search )
  where plain text =
          (trd <$> parseMarkup text '\0') `catch` \(_ :: SomeException) -> return ""
