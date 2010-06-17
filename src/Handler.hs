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
  , once
  , ever
  , keep
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Foldable


data Id m a = Id Int

type Entry m a = (Int, a -> m Bool)
type Entries m a = IntMap (Entry m a)

data Handler m a
  = Handler { nextId  :: Id m a
            , entries :: Entries m a
            }

make :: Handler m a
make = Handler { nextId = Id 0, entries = IntMap.empty }

add :: Monad m => (a -> m Bool) -> Handler m a -> m (Handler m a, Id m a)
add f Handler { nextId = id@(Id n), entries = e } =
  return (h, id)
  where h = Handler { nextId   = Id $ succ n
                    , entries = IntMap.insert n (n, f) e
                    }

remove :: Monad m => Id m a -> Handler m a -> m (Handler m a, ())
remove (Id n) h =
  return (h { entries = IntMap.delete n $ entries h }, ())

invoke :: Monad m => a -> Handler m a -> m (Handler m a, ())
invoke a h@Handler { entries = e } = do
  e' <- foldlM (invoke' a) e e
  return (h { entries = e' }, ())
  where invoke' a e (i, f) = do
          p <- f a
          if p
            then return e
            else return $ IntMap.delete i e

once :: Monad m => (a -> m b) -> a -> m Bool
once = keep False

ever :: Monad m => (a -> m b) -> a -> m Bool
ever = keep True

keep :: Monad m => Bool -> (a -> m b) -> a -> m Bool
keep r f a = f a >> return r
