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
  , AddHandler (..)
  , RemoveHandler (..)
  , InvokeHandler (..)
  , HandlerId
  , HandlerModClass (..)
  , mk
  ) where

import Control.Applicative

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Unique
import Data.IORef


class HandlerClass h a | h -> a where
  applyHandler :: h -> a -> IO ()

instance HandlerClass (a -> IO ()) a where
  applyHandler = ($)

instance HandlerClass (a -> b -> IO ()) (a, b) where
  applyHandler h = uncurry h


data Handler h = Handler (IORef (Seq (Unique, h)))

mkHandler = Handler <$> newIORef Seq.empty

invoke (Handler r) a = F.mapM_ (flip applyHandler a . snd) =<< readIORef r

class HandlerModClass h o a r | h o -> a, h o -> r where
  modifyHandler :: Handler h -> o -> a -> IO r

data HandlerId h = HandlerId Unique


data AddHandler = AddHandler

instance (HandlerClass h a) => HandlerModClass h AddHandler h (HandlerId h) where
  modifyHandler (Handler r) AddHandler f = do
    u <- newUnique
    atomicModifyIORef r $ (, ()) . (|> (u, f))
    return $ HandlerId u

data RemoveHandler = RemoveHandler

instance (HandlerClass h a) => HandlerModClass h RemoveHandler (HandlerId h) () where
  modifyHandler (Handler r) RemoveHandler (HandlerId u) =
    atomicModifyIORef r $ (, ()) . Seq.filter ((/= u) . fst)

data InvokeHandler = InvokeHandler

instance (HandlerClass h a) => HandlerModClass h InvokeHandler a () where
  modifyHandler h InvokeHandler a = invoke h a

mk = do
  h <- mkHandler
  return $ modifyHandler h