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

module Builder
  ( builderEnv
  , runBuilder
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


data Ix = Ix

builderEnv :: Extract Ix Builder
builderEnv = Extract

runBuilder f = do
  builder <- liftIO builderNew
  runIn' (mkEnv Ix builder :*:) $> f

builder = envx Ix

addFromFile file = do
  b <- builder
  liftIO $ builderAddFromFile b file

getObject cast name = do
  b <- builder
  liftIO $ builderGetObject b cast name

getObjectRaw name = do
  b <- builder
  liftIO $ builderGetObjectRaw b name

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
