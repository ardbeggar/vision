-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Jun. 2010
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

module Config
  ( config
  , writeConfig
  , ConfigWidget (..)
  , makeConfigDialog
  , ConfigDialog
  , prepareToShow
  ) where

import Control.Monad

import Data.Maybe

import Text.JSON

import System.FilePath
import System.Directory
import System.IO

import Graphics.UI.Gtk

import Environment
import Compound
import Utils


config name defl = do
  withFile (configFileName name) ReadMode config' `catch` \_ -> return defl
  where config' h = do
          c <- hGetContents h
          case decodeStrict c of
            Ok r    -> return r
            Error _ -> readIO c -- try to read old format

writeConfig name cont =
  writeConfig' >> return True `catch` \_ -> return False
  where writeConfig' = do
          createDirectoryIfMissing True configBaseDir
          writeFile (configFileName name) $ encodeStrict $ showJSON cont


configFileName name = joinPath [configBaseDir, name]

configBaseDir = joinPath [fromJust homeDir, ".vision"]


class CompoundWidget cw => ConfigWidget cw where
  type Config cw
  getConfig    :: cw -> IO (Config cw)
  setConfig    :: cw -> Config cw -> IO ()
  clearConfig  :: cw -> IO ()
  getChanged   :: cw -> IO Bool
  clearChanged :: cw -> IO ()
  grabFocus    :: cw -> IO ()


data ConfigDialog
  = ConfigDialog { cDialog       :: Dialog
                 , prepareToShow :: IO ()
                 }

instance CompoundWidget ConfigDialog where
  type Outer ConfigDialog = Dialog
  outer = cDialog

makeConfigDialog make destroy getc setc = do
  windowGroup <- windowGroupNew

  dialog <- dialogNew
  windowGroupAddWindow windowGroup dialog
  windowSetModal dialog False
  dialogSetHasSeparator dialog False

  dialogAddButton   dialog "gtk-apply"  ResponseApply
  dialogAddButton   dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok"     ResponseOk
  dialogSetResponseSensitive dialog ResponseApply False

  upper <- dialogGetUpper dialog
  cw <- make dialog windowGroup $
    dialogSetResponseSensitive dialog ResponseApply True
  widgetShowAll $ outer cw
  boxPackStartDefaults upper $ outer cw

  let hide = do
        widgetHide dialog
        clearConfig cw
        clearChanged cw
        when destroy $ widgetDestroy dialog
      prepareToShow = do
        setConfig cw =<< getc
        grabFocus cw

  dialog `onResponse` \resp ->
    case resp of
      ResponseApply -> do
        dialogSetResponseSensitive dialog ResponseApply False
        changed <- getChanged cw
        when changed $ setc =<< getConfig cw
        clearChanged cw
        grabFocus cw
      ResponseOk    -> do
        changed <- getChanged cw
        when changed $ setc =<< getConfig cw
        hide
      _ ->
        hide

  return ConfigDialog { cDialog       = dialog
                      , prepareToShow = prepareToShow
                      }
