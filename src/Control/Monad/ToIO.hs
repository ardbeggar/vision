-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 27 Jun. 2011
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

module Control.Monad.ToIO
  ( ToIO (..)
  , io
  ) where

import Control.Monad.Trans
import Control.Monad.W


class (Monad t, MonadIO t) => ToIO t where
  toIO :: t (W t IO)

io :: ToIO m => ((forall a. m a -> IO a) -> IO b) -> m b
io f = do
  W run <- toIO
  liftIO $ f run

instance ToIO IO where
  toIO = return $ W id
