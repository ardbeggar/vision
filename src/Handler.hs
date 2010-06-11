-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 11 Jun. 2010
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

module Handler
  ( Handler
  , mkHandler
  , invoke
  , modifyHandler
  , AddHandler (..)
  , RemoveHandler (..)
  , HandlerId
  ) where

import Control.Applicative

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Unique
import Data.IORef


class HandlerClass h where
  type Args h
  applyHandler :: h -> Args h -> IO ()

instance HandlerClass (a -> IO ()) where
  type Args (a -> IO ()) = a
  applyHandler = ($)

instance HandlerClass (a -> b -> IO ()) where
  type Args (a -> b -> IO ()) = (a, b)
  applyHandler h = uncurry h


newtype (HandlerClass h) => Handler h = Handler (IORef (Seq (Unique, h)))

mkHandler :: (HandlerClass h) => IO (Handler h)
mkHandler = Handler <$> newIORef Seq.empty

invoke :: (HandlerClass h) => Handler h -> Args h -> IO ()
invoke (Handler r) a = F.mapM_ (flip applyHandler a . snd) =<< readIORef r


class (HandlerClass h) => HandlerModClass h o where
  type Arg h o
  type Res h o
  modifyHandler :: Handler h -> o -> Arg h o -> IO (Res h o)


data (HandlerClass h) => HandlerId h = HandlerId Unique


data AddHandler = AddHandler

instance (HandlerClass h) => HandlerModClass h AddHandler where
  type Arg h AddHandler = h
  type Res h AddHandler = HandlerId h
  modifyHandler (Handler r) AddHandler f = do
    u <- newUnique
    atomicModifyIORef r $ (, ()) . (|> (u, f))
    return $ HandlerId u

data RemoveHandler = RemoveHandler

instance (HandlerClass h) => HandlerModClass h RemoveHandler where
  type Arg h RemoveHandler = HandlerId h
  type Res h RemoveHandler = ()
  modifyHandler (Handler r) RemoveHandler (HandlerId u) =
    atomicModifyIORef r $ (, ()) . Seq.filter ((== u) . fst)
