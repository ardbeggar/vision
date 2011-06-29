-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 29 Jun. 2011
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

module Control.Monad.W
  ( W (..)
  , withW
  ) where


newtype W m n = W { runW :: forall a. m a -> n a }

withW :: Monad m => m (W t t1) -> ((forall a. t a -> t1 a) -> m b) -> m b
withW w f = do
  w' <- w
  f $ runW w'
