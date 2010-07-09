-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 8 Jul. 2010
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

module WithMediaInfo
  ( showWithMediaInfo
  , setupOnHide
  , retrieveProperties
  ) where

import Control.Concurrent.MVar
import Control.Monad

import Data.IORef
import Data.List

import Graphics.UI.Gtk hiding (add, remove)

import XMMS
import UI
import Utils
import Handler
import Medialib


showWithMediaInfo visible setup ids =
  modifyMVar_ visible $ \maybeVis ->
    case maybeVis of
      Just vis -> do
        widgetHide vis
        windowSetTransientFor vis window
        windowPresent vis
        return $ Just vis
      Nothing  -> withMediaInfo visible ids $ \list -> do
        maybeNew <- setup list
        fmaybeM_ maybeNew windowPresent
        return maybeNew

setupOnHide visible dialog =
  dialog `onHide` (tryModifyMVar_ visible . const $ return Nothing)

withMediaInfo visible ids f = do
  let ids'       = nub ids
      len        = length ids'
      step       = len `div` 100
      setVisible = modifyMVar_ visible . const . return

  dialog <- dialogNew
  windowSetDefaultSize dialog 200 (-1)
  dialogSetHasSeparator dialog False
  windowSetTitle dialog "Retrieving properties"
  windowSetTransientFor dialog window

  dialogAddButton dialog "gtk-cancel" ResponseCancel

  upper <- dialogGetUpper dialog
  box   <- vBoxNew False 0
  containerSetBorderWidth box 7
  boxPackStart upper box PackNatural 0

  pbar <- progressBarNew
  progressBarSetText pbar "Retrieving properties"
  boxPackStart box pbar PackNatural 0

  ref <- newIORef (0, ids', [])
  hid <- onMediaInfo . add $ \(id, _, info) -> do
    (ctr, todo, ready) <- readIORef ref
    case todo of
      (i:is) | i == id -> do
        if null is
          then do
          setVisible =<< (f $ reverse ((id, info) : ready))
          widgetDestroy dialog
          return False
          else do
          let ctr' = ctr + 1
          when (step == 0 || ctr' `mod` step == 0) $
            progressBarSetFraction pbar $
              fromIntegral ctr' / fromIntegral len
          requestInfo $ head is
          writeIORef ref (ctr', is, (id, info) : ready)
          return True
      _ ->
        return True

  dialog `onResponse` \_ -> do
    onMediaInfo $ remove hid
    widgetDestroy dialog
    setVisible Nothing

  did <- onServerConnection . add . once $ \conn ->
    unless conn $ do
      flip timeoutAdd 0 $ do
        onMediaInfo $ remove hid
        widgetDestroy dialog
        setVisible Nothing
        return False
      return ()
  dialog `onDestroy` (onServerConnection $ remove did)

  widgetShowAll dialog
  requestInfo $ head ids

  return $ Just dialog


retrieveProperties pbar ids f = do
  let ids'       = nub ids
      len        = length ids'
      step       = len `div` 100

  ref <- newIORef (0, ids', [])
  hid <- onMediaInfo . add $ \(id, _, info) -> do
    (ctr, todo, ready) <- readIORef ref
    case todo of
      (i:is) | i == id -> do
        if null is
          then do
          f $ reverse ((id, info) : ready)
          return False
          else do
          let ctr' = ctr + 1
          when (step == 0 || ctr' `mod` step == 0) $
            progressBarSetFraction pbar $
              fromIntegral ctr' / fromIntegral len
          requestInfo $ head is
          writeIORef ref (ctr', is, (id, info) : ready)
          return True
      _ ->
        return True

  progressBarSetFraction pbar 0
  progressBarSetText pbar "Retrieving properties"
  requestInfo $ head ids

  return . onMediaInfo $ remove hid
