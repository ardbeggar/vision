-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
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

module Playtime
  ( initPlaytime
  , currentId
  , makeSeekControl
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map

import Graphics.UI.Gtk hiding (add, remove, get)

import XMMS2.Client hiding (playbackStatus)

import XMMS
import Medialib
import Context
import Utils
import Playback


data Playtime
  = Playtime { pAdj        :: Adjustment
             , pCurrentIdV :: TVar (Maybe MediaId)
             }

adj        = pAdj context
currentId  = readTVar currentIdV
currentIdV = pCurrentIdV context


initPlaytime = do
  context <- initContext
  let ?context = context

  rcV <- atomically $ newTVar Nothing
  ptV <- atomically $ newTVar 0
  ptW <- atomically $ newEmptyTWatch ptV
  psW <- atomically $ newEmptyTWatch playbackStatus

  let ptRq = forever $ do
        atomically $ do
          rc <- readTVar rcV
          if rc == Just 0
            then return ()
            else retry
        playbackPlaytime xmms >>* do
          pt <- catchResult 0 fromIntegral
          liftIO $ atomically $ writeTVar ptV pt
        threadDelay 1000000

  cId <- adj `onValueChanged` do
    atomically $ do
      rc <- readTVar rcV
      withJust rc $ \rc ->
        writeTVar rcV $ Just (rc + 1)
    v <- adjustmentGetValue adj
    playbackSeekMs xmms (round v) SeekSet >>* do
      liftIO $ atomically $ do
        rc <- readTVar rcV
        withJust rc $ \rc ->
          writeTVar rcV $ Just (rc - 1)

  xcN <- atomically $ mkSN
  atomically $ activateSN xcN
  forkIO $ evalPTM cId $ xmmsNC ptW psW xcN rcV
  forkIO $ ptRq

  return ?context

handleCurrentId = do
  ci <- catchResult Nothing Just
  liftIO $ atomically $ writeTVar currentIdV ci

initContext = do
  adj        <- adjustmentNew 0 0 0 5000 5000 0
  currentIdV <- newTVarIO Nothing
  return $ augmentContext
    Playtime { pAdj        = adj
             , pCurrentIdV = currentIdV
             }

makeSeekControl = do
  view <- hScaleNew adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  w <- atomically $ newEmptyTWatch playbackStatus
  t <- forkIO $ forever $ do
    s <- atomically $ watch w
    widgetSetSensitive view $ s == (Just StatusPlay)

  view `onDestroy` (killThread t)

  return view


data S =
  S { sPt :: Int
    , sId :: Maybe MediaId
    , sTd :: Int
    , sPs :: PlaybackStatus
    , sCi :: ConnectId Adjustment
    }

mkS id cId =
  S { sPt = 0
    , sId = id
    , sTd = 0
    , sPs = StatusStop
    , sCi = cId
    }

evalPTM cId f =
  evalStateT f (mkS Nothing cId)

withoutSeek f = do
  cId <- gets sCi
  liftIO $ bracket_
    (signalBlock cId)
    (signalUnblock cId)
    (liftIO f)

data Msg
  = PT Int
  | CI (Maybe MediaId)
  | MI (MediaId, Stamp, MediaInfo)
  | PS (Maybe PlaybackStatus)
  | XC Bool

xmmsNC ptW psW xcN rcV = do
  conn <- liftIO $ atomically $ waitSN xcN
  if conn
    then do
    (miC, ciW) <- liftIO $ do
      playbackCurrentId xmms >>*
        handleCurrentId
      broadcastPlaybackCurrentId xmms >>*
        (handleCurrentId >> persist)
      atomically $ writeTVar rcV $ Just 0
      atomically $ doneSN xcN
      miC <- atomically $ dupTChan mediaInfoChan
      ciW <- atomically $ newTWatch currentIdV Nothing
      return (miC, ciW)
    xmmsC ciW ptW miC psW xcN rcV
    else
    xmmsNC ptW psW xcN rcV

xmmsC ciW ptW miC psW xcN rcV = do
  msg <- liftIO $ atomically $
         msum [ XC <$> waitSN xcN
              , PS <$> watch psW
              , CI <$> watch ciW
              , PT <$> watch ptW
              , MI <$> readTChan miC
              ]
  case msg of
    XC False -> do
      cId <- gets sCi
      put $ mkS Nothing cId
      liftIO $ do
        atomically $ writeTVar rcV Nothing
        atomically $ doneSN xcN
      xmmsNC ptW psW xcN rcV
    PT pt -> do
      handlePT pt
      xmmsC ciW ptW miC psW xcN rcV
    CI ci -> do
      handleCI ci
      xmmsC ciW ptW miC psW xcN rcV
    MI mi -> do
      handleMI mi
      xmmsC ciW ptW miC psW xcN rcV
    PS ps -> do
      handlePS $ fromMaybe StatusStop ps
      xmmsC ciW ptW miC psW xcN rcV

handlePT pt = do
  s <- get
  put s { sPt = pt }
  unless (sTd s == 0 || sPs s == StatusStop) $
    withoutSeek $ adjustmentSetValue adj $ fromIntegral pt

handleCI id = do
  modify $ \s ->
    s { sPt = 0
      , sId = id
      , sTd = 0
      }
  withoutSeek $ do
    adjustmentSetValue adj 0
    adjustmentSetUpper adj 0
  liftIO $ withJust id $ requestInfo Current

handleMI (id, _, info) = do
  s <- get
  when (sId s == Just id) $
    case Map.lookup "duration" info of
      Just (PropInt32 d) -> do
        put s { sTd = fromIntegral d }
        withoutSeek $ do
          adjustmentSetUpper adj $ fromIntegral d
          unless (sPs s == StatusStop) $
            adjustmentSetValue adj $ fromIntegral $ sPt s
      _ -> return ()

handlePS StatusStop = do
  modify $ \s ->
    s { sPs = StatusStop
      , sPt = 0
      }
  withoutSeek $ adjustmentSetValue adj 0
handlePS ps =
  modify $ \s -> s { sPs = ps }
