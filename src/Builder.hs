-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2011
--
--  Copyright (C) 2011 Oleg Belozeorov
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

{-# LANGUAGE Rank2Types #-}

module Builder
  ( WithBuilder
  , withBuilder
  , builder
  , addFromFile
  , getObject
  , getObjectRaw
  , action
  , actions
  , bindAction
  , bindActions
  , maybeBindAction
  ) where

import Control.Applicative

import Graphics.UI.Gtk


type WithBuilder = ?_Builder :: Builder

withBuilder :: (WithBuilder => IO a) -> IO a
withBuilder func = do
  builder <- builderNew
  let ?_Builder = builder
  func

builder :: WithBuilder => Builder
builder = ?_Builder

addFromFile :: WithBuilder => FilePath -> IO ()
addFromFile file =
  builderAddFromFile builder file

getObject :: (WithBuilder, GObjectClass o) => (GObject -> o) -> String -> IO o
getObject cast name =
  builderGetObject builder cast name

getObjectRaw :: WithBuilder => String -> IO (Maybe GObject)
getObjectRaw name =
  builderGetObjectRaw builder name

action :: WithBuilder => String -> IO Action
action = getObject castToAction

actions :: WithBuilder => [String] -> IO [Action]
actions = mapM action

bindAction :: WithBuilder => String -> IO () -> IO (ConnectId Action)
bindAction name func = do
  a <- action name
  a `on` actionActivated $ func

bindActions :: WithBuilder => [(String, IO ())] -> IO [ConnectId Action]
bindActions = mapM $ uncurry bindAction

maybeBindAction :: WithBuilder => String -> IO () -> IO (Maybe (ConnectId Action))
maybeBindAction name func = do
  ma <- getObjectRaw name
  case ma of
    Just a  -> Just <$> on (castToAction a) actionActivated func
    Nothing -> return Nothing
