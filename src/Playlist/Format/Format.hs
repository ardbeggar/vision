-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 23 Feb. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module Playlist.Format.Format
  ( Fe (..)
  , cookFormat
  , formatMediaInfo
  ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Error

import Graphics.UI.Gtk (escapeMarkup)

import Properties


data Fe a
  = FeP a
  | FeL String
  | FeO [Fe a]


cookFormat fmt = runErrorT (mapM f fmt)
  where f (FeP n) =
          maybe (throwError "") (return . FeP) =<< liftIO (property n)
        f (FeL s) =
          return $ FeL s
        f (FeO l) =
          FeO <$> mapM f l

formatMediaInfo [] _         = Nothing
formatMediaInfo (fmt:fmts) m =
  execWriterT (mapM_ f fmt) `mplus` formatMediaInfo fmts m
  where f (FeP p) = tell . escapeMarkup =<< lift (lookup p m)
        f (FeL s) = tell s
        f (FeO l) = mapM_ f l `mplus` return ()
