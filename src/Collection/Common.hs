-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 14 Jul. 2010
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

{-# LANGUAGE Rank2Types #-}

module Collection.Common
  ( Com (..)
  , coms
  , withCommon
  , addView
  , mergeUI
  , removeUI
  , newUITag
  , FocusChild (..)
  , withSelectedView
  ) where

import Control.Applicative
import Control.Monad

import Data.IORef
import Data.Maybe

import Graphics.UI.Gtk hiding (focus)

import UI
import Utils
import Compound

import Collection.ScrollBox
import Collection.ComboModel


data Com
  = Com { eSBox   :: ScrollBox
        , eVPopup :: Menu
        , eCModel :: ListStore ComboItem
        , eScroll :: ScrolledWindow
        , eSAdj   :: Adjustment
        , eUITag  :: IORef Integer
        , eUIRef  :: IORef (Maybe (Integer, ActionGroup, Maybe MergeId))
        }

coms = ($ ?_Collection_Common)

newtype Wrap a = Wrap { unWrap :: (?_Collection_Common :: Com) => a }

withCommon    = withCommon' . Wrap
withCommon' w = do
  vpopup <- getWidget castToMenu "ui/view-popup"

  sbox   <- mkScrollBox
  cmodel <- mkModel

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetShadowType scroll ShadowNone
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyNever
  adj <- scrolledWindowGetHAdjustment scroll

  fcRef <- newIORef Nothing

  let scrollIn mfc = void $ withJust mfc $ \fc ->
        flip idleAdd priorityLow $ do
          Rectangle x _ w _ <- widgetGetAllocation fc
          adjustmentClampPage adj (fromIntegral x) (fromIntegral (x + w))
          return False

  adj `afterAdjChanged` do
    mfc <- readIORef fcRef
    scrollIn mfc

  (sBox sbox) `on` setFocusChild $ \mfc -> do
    writeIORef fcRef mfc
    scrollIn mfc

  containerAdd scroll $ outer sbox

  uiTag <- newIORef 0
  uiRef <- newIORef Nothing

  let ?_Collection_Common =
        Com { eSBox   = sbox
            , eVPopup = vpopup
            , eCModel = cmodel
            , eScroll = scroll
            , eSAdj   = adj
            , eUITag  = uiTag
            , eUIRef  = uiRef
            }

  unWrap w


addView w = do
  scrollBoxAdd (coms eSBox) $ outer w
  widgetGrabFocus $ focus w

class FocusChild f where
  type Focus f
  focus :: f -> Focus f

withSelectedView combo f = do
  iter <- comboBoxGetActiveIter combo
  withJust iter $ \iter -> do
    v <- listStoreGetValue (coms eCModel) $ listStoreIterToIndex iter
    f v


class UIDefClass def where
  mergeUIDef :: UIManager -> def -> IO MergeId

instance UIDefClass String where
  mergeUIDef = uiManagerAddUiFromString

instance UIDefClass [(String, [Maybe String])] where
  mergeUIDef uiManager list = do
    mergeId <- uiManagerNewMergeId uiManager
    forM_ list $ \(path, actions) ->
      forM_ actions $ add mergeId path
    return mergeId
    where add mergeId path (Just action) =
            uiManagerAddUi uiManager mergeId path action (Just action) [UiManagerAuto] False
          add mergeId path Nothing =
            uiManagerAddUi uiManager mergeId path "" Nothing [UiManagerAuto] False

mergeUI tag ag ui = do
  mui <- readIORef (coms eUIRef)
  withJust mui $ \(_, ag, mmid) -> do
    uiManagerRemoveActionGroup uiManager ag
    withJust mmid $ \mid ->
      uiManagerRemoveUi uiManager mid
  uiManagerInsertActionGroup uiManager ag 0
  mid <- case ui of
    Just def -> Just <$> mergeUIDef uiManager def
    Nothing  -> return Nothing
  writeIORef (coms eUIRef) $ Just (tag, ag, mid)

removeUI tag = do
  mui <- readIORef (coms eUIRef)
  withJust mui $ \(tag', ag, mmid) -> do
    when (fromMaybe tag' tag == tag') $ do
      uiManagerRemoveActionGroup uiManager ag
      withJust mmid $ \mid ->
        uiManagerRemoveUi uiManager mid
      writeIORef (coms eUIRef) Nothing

newUITag = do
  tag <- readIORef (coms eUITag)
  writeIORef (coms eUITag) $ tag + 1
  return tag
