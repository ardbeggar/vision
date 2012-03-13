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
  , comWithColl
  , comWithIds
  , comWithSel
  , comWithNames
  , addView
  , mergeUI
  , removeUI
  , newUITag
  , FocusChild (..)
  , withSelectedView
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import Graphics.UI.Gtk hiding (focus)

import XMMS2.Client

import UI
import Utils
import XMMS
import Compound
import Builder

import Collection.Actions
import Collection.ScrollBox
import Collection.ComboModel


data Com
  = Com { eABRef  :: IORef ActionBackend
        , eAE     :: ActionEnabler
        , eSBox   :: ScrollBox
        , eLPopup :: Menu
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
  abRef <- newIORef emptyAB

  selActs <- actions [ "add-to-playlist"
                     , "replace-playlist"
                     , "copy"
                     , "edit-properties"
                     , "export-properties"
                     , "save-collection"
                     ]
  renAct <- action "rename-collection"
  delAct <- action "delete-collections"
  let ae = AE { aEnableSel = \en -> mapM_ (`actionSetSensitive` en) selActs
              , aEnableRen = actionSetSensitive renAct
              , aEnableDel = actionSetSensitive delAct
              }

  lpopup <- getWidget castToMenu "ui/list-popup"
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
        Com { eABRef  = abRef
            , eAE     = ae
            , eSBox   = sbox
            , eLPopup = lpopup
            , eVPopup = vpopup
            , eCModel = cmodel
            , eScroll = scroll
            , eSAdj   = adj
            , eUITag  = uiTag
            , eUIRef  = uiRef
            }

  unWrap w


comWithColl f = do
  ab <- readIORef $ coms eABRef
  aWithColl ab f

comWithIds f =
  comWithColl $ \coll ->
    collQueryIds xmms coll [] 0 0 >>* do
      ids <- result
      liftIO $ f ids

comWithSel f = do
  ab <- readIORef $ coms eABRef
  withJust (aSelection ab) f

comWithNames f = do
  ab <- readIORef $ coms eABRef
  aWithNames ab f

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
    when (tag == tag') $ do
      uiManagerRemoveActionGroup uiManager ag
      withJust mmid $ \mid ->
        uiManagerRemoveUi uiManager mid
      writeIORef (coms eUIRef) Nothing

newUITag = do
  tag <- readIORef (coms eUITag)
  writeIORef (coms eUITag) $ tag + 1
  return tag
