-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Feb. 2010
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

module Playlist.Format
  ( initFormat
  , makeTrackInfo
  , getFormatDefs
  , putFormatDefs
  , TrackInfo (..)
  , trackInfoAttrs
  , trackInfoText
  , trackInfoDuration
  ) where

import Prelude hiding (lookup)

import Control.Applicative

import Data.IORef
import Data.Maybe
import Data.Either
import Data.Char (toLower)

import Graphics.UI.Gtk
import System.Glib.GError

import Medialib
import Properties
import Config
import Utils
import Env

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


data Format
  = Format { fMakeInfoRef      :: IORef (MediaInfo -> IO ([AttrOp CellRendererText], String))
           , fFormatDefsRef    :: IORef [String]
           , fLookupDuration   :: MediaInfo -> String
           , fLookupURL        :: MediaInfo -> String
           }

getMakeInfo = readIORef (fMakeInfoRef getEnv)
putMakeInfo = writeIORef (fMakeInfoRef getEnv)

getFormatDefs  = readIORef (fFormatDefsRef getEnv)
putFormatDefs' = writeIORef (fFormatDefsRef getEnv)
putFormatDefs defs = do
  putFormatDefs' defs
  saveFormatDefs
  updateFormats True

lookupDuration = fLookupDuration getEnv
lookupURL      = fLookupURL getEnv

initFormat = do
  env <- initEnv
  let ?env = env

  loadFormatDefs

  return ?env

initEnv = do
  formatDefsRef    <- newIORef []
  makeInfoRef      <- newIORef $ const $ return ([], "")
  duration         <- fromJust <$> property "Duration"
  url              <- fromJust <$> property "URL"
  return $ augmentEnv
    Format { fMakeInfoRef      = makeInfoRef
           , fFormatDefsRef    = formatDefsRef
           , fLookupDuration   = maybe "" escapeMarkup . lookup duration
           , fLookupURL        = maybe "" escapeMarkup . lookup url
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
          , "[{Track} ]<b>{Title}</b>\n\
            \{Artist} — {Album}" ]

saveFormatDefs = do
  writeConfig "playlist-formats.conf" =<< getFormatDefs
  return ()

getFormats = (rights . map parseFormat) <$> getFormatDefs

updateFormats _ = do
  putMakeInfo =<< makeMakeInfo =<< getFormats

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
          (trd <$> parseMarkup text '\0') `catchGError` \_ -> return ""
