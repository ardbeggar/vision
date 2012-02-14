-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 22 Jun. 2010
--
--  Copyright (C) 2010, 2011 Oleg Belozeorov
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

{-# LANGUAGE DeriveDataTypeable, Rank2Types #-}

module Playtime
  ( initPlaytime
  , withPlaytime
  , makeSeekControl
  , playtimeEnv
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TWatch
import Control.Concurrent.STM.TGVar

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map
import Data.Typeable
import Data.Env

import Graphics.UI.Gtk hiding (add, remove, get)

import XMMS2.Client hiding (playbackStatus)

import XMMS
import Medialib
import Utils
import Playback
import Registry


data Playtime
  = Playtime { _adj        :: Adjustment
             , _currentIdV :: TVar (Maybe MediaId)
             , _playtimeV  :: TVar Int
             , _seekCountV :: TVar (Maybe Int)
             }
    deriving (Typeable)

adj        = _adj ?playtime
currentIdV = _currentIdV ?playtime
playtimeV  = _playtimeV ?playtime
seekCountV = _seekCountV ?playtime


data Ix = Ix deriving (Typeable)

playtimeEnv :: Extract Ix Playtime
playtimeEnv = Extract

initPlaytime = withXMMS $ withMedialib $ withPlayback $ do
  pt <- makePlaytime
  addEnv Ix pt
  let ?playtime = pt

  cId <- setupSeek
  xcW <- atomically $ newTGWatch connectedV
  let mon xc
        | xc = do
          rt <- forkIO requestPlaytime
          ct <- forkIO $ evalPTM cId dispatch
          xc <- atomically $ watch xcW
          killThread rt
          killThread ct
          atomically $ writeTVar seekCountV Nothing
          mon xc
        | otherwise = do
          xc <- atomically $ watch xcW
          mon xc
  forkIO $ mon False

  return ()


newtype Wrap a = Wrap { unWrap :: (?playtime :: Playtime) => a }

withPlaytime    = withPlaytime' . Wrap
withPlaytime' w = do
  Just (Env pt) <- getEnv playtimeEnv
  let ?playtime = pt in unWrap w

makePlaytime = do
  adj        <- adjustmentNew 0 0 0 5000 5000 0
  currentIdV <- newTVarIO Nothing
  playtimeV  <- newTVarIO 0
  seekCountV <- newTVarIO Nothing
  return $ Playtime { _adj        = adj
                    , _currentIdV = currentIdV
                    , _playtimeV  = playtimeV
                    , _seekCountV = seekCountV
                    }

requestPlaytime = forever $ do
  checkSeekCount
  playbackPlaytime xmms >>* do
    pt <- catchResult 0 fromIntegral
    liftIO $ atomically $ writeTVar playtimeV pt
  threadDelay 1000000

checkSeekCount = atomically $ do
  rc <- readTVar seekCountV
  if rc == Just 0
    then return ()
    else retry

modSeekCount op = atomically $ do
  rc <- readTVar seekCountV
  withJust rc $ \rc ->
    writeTVar seekCountV $ Just $ op rc

handleCurrentId = do
  ci <- catchResult Nothing Just
  liftIO $ atomically $ writeTVar currentIdV ci

setupSeek =  adj `onValueChanged` do
  modSeekCount (+ 1)
  v <- adjustmentGetValue adj
  playbackSeekMs xmms (round v) SeekSet >>* do
    liftIO $ modSeekCount $ \n -> n - 1

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

dispatch = do
  (miC, ciW, psW, ptW) <- liftIO $ do
    playbackCurrentId xmms >>*
      handleCurrentId
    broadcastPlaybackCurrentId xmms >>*
      (handleCurrentId >> persist)
    atomically $ writeTVar seekCountV $ Just 0
    miC <- atomically $ dupTChan mediaInfoChan
    ciW <- atomically $ newTWatch currentIdV Nothing
    psW <- atomically $ newEmptyTWatch playbackStatus
    ptW <- atomically $ newEmptyTWatch playtimeV
    return (miC, ciW, psW, ptW)
  forever $ do
    msg <- liftIO $ atomically $
           msum [ PS <$> watch psW
                , CI <$> watch ciW
                , PT <$> watch ptW
                , MI <$> readTChan miC
                ]
    case msg of
      PT pt -> handlePT pt
      CI ci -> handleCI ci
      MI mi -> handleMI mi
      PS ps -> handlePS $ fromMaybe StatusStop ps

handlePT pt = do
  s <- get
  unless (sPs s == StatusStop) $ do
    put s { sPt = pt }
    unless (sTd s == 0) $
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
