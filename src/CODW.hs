-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

module CODW
  ( CODW
  , makeCODW
  , showCODW
  ) where

import Control.Concurrent.MVar

import Graphics.UI.Gtk

import UI


data WindowClass w => CODW d w
  = CODW { cMake :: d -> IO w
         , cMVar :: MVar (Maybe w)
         }

makeCODW make = do
  mVar <- newMVar Nothing
  return CODW { cMake = make
              , cMVar = mVar
              }

showCODW dta codw =
  modifyMVar_ (cMVar codw) $ \maybeW -> do
    w <- case maybeW of
      Just w  ->
        return w
      Nothing -> do
        w <- cMake codw dta
        w `onDestroy` do
          modifyMVar_ (cMVar codw) $ const $ return Nothing
        return w
    windowSetTransientFor w window
    widgetHide w
    windowPresent w
    return $ Just w
