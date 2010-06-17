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
  ( Id
  , Handler
  , make
  , add
  , remove
  , invoke
  , withHandler
  , once
  , ever
  , keep
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Foldable
import Control.Applicative


data Id m a = Id Int

type Entry m a = (Int, a -> m Bool)
type Entries m a = IntMap (Entry m a)

data Handler m a
  = Handler { nextId  :: Id m a
            , entries :: Entries m a
            }

make :: Handler m a
make = Handler { nextId = Id 0, entries = IntMap.empty }

add :: (a -> m Bool) -> Handler m a -> (Handler m a, Id m a)
add f Handler { nextId = id@(Id n), entries = h } = (handler, id)
  where handler = Handler { nextId   = Id $ succ n
                          , entries = IntMap.insert n (n, f) h
                          }

remove :: Id m a -> Handler m a -> Handler m a
remove (Id n) h = h { entries = IntMap.delete n $ entries h }

invoke :: Monad m => a -> Handler m a -> m (Handler m a)
invoke a handler@Handler { entries = e } = do
  e' <- foldlM (invoke' a) e e
  return handler { entries = e' }

invoke' :: Monad m => a -> Entries m a -> (Entry m a) -> m (Entries m a)
invoke' a e (i, f) = do
  p <- f a
  if p
    then return e
    else return $ IntMap.delete i e

class HandlerSource m a b s | s a m -> b where
  handler :: s -> m (Handler m a, b)

instance Monad m => HandlerSource m a b (Handler m a, b) where
  handler = return

instance Monad m => HandlerSource m a () (Handler m a) where
  handler = return . (, ())

instance (Functor m) => HandlerSource m a () (m (Handler m a)) where
  handler s = (, ()) <$> s

withHandler :: HandlerSource m a b s => ((Handler m a -> m (Handler m a, b)) -> m b) -> (Handler m a -> s) -> m b
withHandler with f = with  (handler . f)

once :: Monad m => (a -> m b) -> a -> m Bool
once = keep False

ever :: Monad m => (a -> m b) -> a -> m Bool
ever = keep True

keep :: Monad m => Bool -> (a -> m b) -> a -> m Bool
keep r f a = f a >> return r
