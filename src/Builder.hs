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

{-# LANGUAGE RankNTypes #-}

module Builder
  ( withBuilder
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
import Control.Monad.EnvIO

import Graphics.UI.Gtk


newtype Wrap a = Wrap { unWrap :: (?builder :: Builder) => a }

withBuilder    = withBuilder' . Wrap
withBuilder' w = do
  builder <- liftIO $ builderNew
  let ?builder = builder in unWrap w

builder = ?builder

addFromFile file =
  liftIO $ builderAddFromFile builder file

getObject cast name =
  liftIO $ builderGetObject builder cast name

getObjectRaw name =
  liftIO $ builderGetObjectRaw builder name

action = getObject castToAction

actions = mapM action

bindAction name func = do
  a <- action name
  liftIO $ a `on` actionActivated $ func

bindActions = mapM $ uncurry bindAction

maybeBindAction name func = do
  ma <- getObjectRaw name
  liftIO $ case ma of
    Just a  -> Just <$> on (castToAction a) actionActivated func
    Nothing -> return Nothing
