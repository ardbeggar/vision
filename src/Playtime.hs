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
  = Playtime { pAdj :: Adjustment }

adj = pAdj context

initPlaytime = do
  context <- initContext
  let ?context = context

  rcV <- atomically $ newTVar Nothing
  ciV <- atomically $ newTVar Nothing
  ciW <- atomically $ newTWatch ciV Nothing
  ptV <- atomically $ newTVar 0
  ptW <- atomically $ newEmptyTWatch ptV
  miC <- atomically $ dupTChan mediaInfoChan
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
  forkIO $ evalPTM cId $ forever $ handleMsg ciV ciW ptW miC psW xcN rcV
  forkIO $ ptRq

  return ?context

handleCurrentId ciV = do
  ci <- catchResult Nothing Just
  liftIO $ atomically $ writeTVar ciV ci

initContext = do
  adj <- adjustmentNew 0 0 0 5000 5000 0
  return $ augmentContext
    Playtime { pAdj = adj }

makeSeekControl = do
  view <- hScaleNew adj
  scaleSetDrawValue view False
  rangeSetUpdatePolicy view UpdateContinuous
  widgetSetCanFocus view False

  vw <- atomically newEmptyTMVar
  view `onDestroy` (atomically $ putTMVar vw ())
  sw <- atomically $ newEmptyTWatch playbackStatus
  let mgr = do
        msg <- atomically $ msum [Left <$> takeTMVar vw, Right <$> watch sw]
        case msg of
          Left _  -> return ()
          Right s -> do
            widgetSetSensitive view $ s == (Just StatusPlay)
            mgr
  forkIO mgr

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

handleMsg ciV ciW ptW miC psW xcN rcV = do
  msg <- liftIO $ atomically $
         msum [ XC <$> waitSN xcN
              , PS <$> watch psW
              , CI <$> watch ciW
              , PT <$> watch ptW
              , MI <$> readTChan miC
              ]
  case msg of
    XC xc -> handleXC xc xcN ciV rcV
    PT pt -> handlePT pt
    CI ci -> handleCI ci
    MI mi -> handleMI mi
    PS ps -> handlePS $ fromMaybe StatusStop ps

handleXC xc xcN ciV rcV = do
  liftIO $ putStrLn $ "connected: " ++ show xc
  if xc
    then liftIO $ do
    playbackCurrentId xmms >>* handleCurrentId ciV
    broadcastPlaybackCurrentId xmms >>* (handleCurrentId ciV >> persist)
    atomically $ writeTVar rcV $ Just 0
    else do
    cId <- gets sCi
    put $ mkS Nothing cId
    liftIO $ atomically $ writeTVar rcV Nothing
  liftIO $ putStrLn "handled"
  liftIO $ atomically $ doneSN xcN

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
  liftIO $ withJust id requestInfo

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
