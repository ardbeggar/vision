-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 3 Oct. 2009
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

module Properties.Editor
  ( setupEditor
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (State, withState)

import Control.Concurrent

import Data.IORef
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int
import Data.Array
import Data.List (unionBy)

import XMMS2.Client hiding (Property)
import qualified XMMS2.Client.Types as X

import Graphics.UI.Gtk hiding (get)

import UI
import Utils
import Medialib
import Config
import XMMS

import Properties.Property
import Properties.Model hiding (addProperty)


setupEditor = do
  group <- actionGroupNew "property-editor"
  insertActionGroup group 1

  edit <- actionNew "edit-properties" "_Edit properties…"
          Nothing (Just stockEdit)
  actionGroupAddActionWithAccel group edit $ Just "<Alt>Return"

  actionSetSensitive edit False
  onIdsSelected $ actionSetSensitive edit . not . null

  showEditor <- makePropertyEditor
  edit `on` actionActivated $ showEditor

  return ()


data Context
  = Context { eLock     :: MVar ()
        , eStateRef :: IORef (Maybe State)
        , eDialog   :: Dialog
        , eStore    :: ListStore (Property, Maybe X.Property)
        , ePrevB    :: Button
        , eNextB    :: Button }

tryLock f = maybe (return ()) (const f) =<< tryTakeMVar (eLock ?context)
unlock    = putMVar (eLock ?context) ()

initState     = writeIORef (eStateRef ?context) . Just =<< makeState
withState fun = do
  state <- readIORef (eStateRef ?context)
  fmaybeM_ state $ \state ->
    writeIORef (eStateRef ?context) . Just =<< execStateT fun state

dialog      = eDialog ?context
enableApply = liftIO . dialogSetResponseSensitive dialog ResponseApply

store          = eStore ?context
storeToList    = liftIO $ listStoreToList store
storeToNumList = zip [0 .. ] <$> storeToList
storeSet n     = liftIO . listStoreSetValue store n
storeGet       = liftIO . listStoreGetValue store
storeAppend    = liftIO . listStoreAppend store
storeRemove    = liftIO . listStoreRemove store


data State
  = State { sIdVec    :: Array Int Int32
          , sCurIx    :: Int
          , sInfos    :: Map Int32 (MediaInfo, MediaInfo)
          , sCchgs    :: MediaInfo
          , sPerTrack :: Bool }

togglePerTrack = do
  mergeChanges
  modify $ \state ->
    state { sPerTrack = not $ sPerTrack state }
  updateModel

makeState = do
  ids   <- getSelectedIds
  infos <- mapM mk ids
  return State { sIdVec    = listArray (0, length ids - 1) ids
               , sCurIx    = 0
               , sInfos    = Map.fromList infos
               , sCchgs    = Map.empty
               , sPerTrack = True }
  where mk i = ((i, ) . (, Map.empty) . (maybe Map.empty id)) <$> getInfo i


updateModel = do
  setupPrevNext
  populateStore
  modify $ \state ->
    state { sCchgs = Map.empty }

setupPrevNext = do
  state <- get
  let (prev, next) =
        if sPerTrack state
        then let cur = sCurIx state in
        (cur > 0, cur < (snd $ bounds $ sIdVec state))
        else (False, False)
  liftIO $ do
    widgetSetSensitive (ePrevB ?context) prev
    widgetSetSensitive (eNextB ?context) next

populateStore = do
  info <- getCurrentInfo
  mapM_ (\(n, (p, _)) ->
          storeSet n (p, Map.lookup (propKey p) info)
        ) =<< storeToNumList

getCurrentInfo = do
  state <- get
  return $
    if sPerTrack state
    then mergeInfo  $ sInfos state Map.! (sIdVec state ! sCurIx state)
    else commonInfo $ sInfos state

mergeInfo = uncurry $ flip Map.union

commonInfo infos =
  let ci x y = Map.intersectionWith ci' (mergeInfo x) y
      ci' x (Just y) | x == y = Just y
      ci' _ _                 = Nothing
      Just (info, infos') = Map.minView infos
  in Map.mapMaybe id $ Map.fold ci (Map.map Just (mergeInfo info)) infos'


nextTrack     = stepTrack succ
prevTrack     = stepTrack pred
stepTrack upd = do
  mergeChanges
  modify $ \state ->
    state { sCurIx = upd $ sCurIx state }
  updateModel

mergeChanges = modify $ \state ->
  let mc     = (mapSnd . Map.union) (sCchgs state)
      infos  = sInfos state
      infos' =
        if sPerTrack state
        then Map.adjust mc (sIdVec state ! sCurIx state) infos
        else Map.map mc infos
  in state { sInfos = infos', sCchgs = Map.empty }


applyChanges = do
  changes <- getAndCommitChanges
  liftIO $ mapM_ (\(i, c) -> mapM_ (set i) (Map.toList c)) changes
  where
    set id (k, v) = do
      medialibEntryPropertySet xmms id (Just "client/generic/override") k v
      return ()

getAndCommitChanges = do
  mergeChanges
  state <- get
  let (changes, infos) =
        Map.mapAccumWithKey (\acc k (i, c) ->
                              ((k, c) : acc, (Map.union c i, Map.empty))
                            ) [] $ sInfos state
  put state { sInfos = infos }
  return changes


addProperty p = do
  storeAppend =<< (p, ) . Map.lookup (propKey p) <$> getCurrentInfo
  return ()

removeProperty p = do
  (_, rem) <- foldM (\(n, rem) (p', _) ->
                      if propName p == propName p'
                      then do
                        storeRemove n
                        return (n, Map.insert (propKey p) 0 rem)
                      else
                        return (n + 1, rem)
                    ) (0, Map.empty) =<< storeToList
  state <- get
  let rp            = (flip Map.difference) rem
      changes       = rp $ sCchgs state
      (null, infos) =
        Map.mapAccum (\n (i, c) ->
                       let c' = rp c in
                       (n && Map.null c', (i, c'))
                     ) (Map.null changes) (sInfos state)
  enableApply $ not null
  put state { sInfos = infos, sCchgs = changes }

savePropertyEditorConfig = do
  writeConfig "property-editor.conf" =<< map (propName . fst) <$> storeToList
  return ()

makePropertyEditor = do
  -- Initialize our contextironment.
  lock     <- newMVar ()
  stateRef <- newIORef Nothing
  dialog   <- dialogNew
  store    <- listStoreNew []
  prevB    <- buttonNewWithMnemonic "_Previous track"
  nextB    <- buttonNewWithMnemonic "_Next track"
  ptrkB    <- checkButtonNewWithMnemonic "Per _track"
  let ?context = Context { eLock     = lock
                 , eStateRef = stateRef
                 , eDialog   = dialog
                 , eStore    = store
                 , ePrevB    = prevB
                 , eNextB    = nextB }

  -- Set up dialog.
  windowSetTransientFor dialog mainWindow
  windowSetModal dialog False
  windowSetTitle dialog "Edit properties"
  windowSetDefaultSize dialog 800 600
  dialogSetHasSeparator dialog False

  dialogAddButton   dialog "gtk-apply"  ResponseApply
  dialogAddButton   dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok"     ResponseOk
  setupOkCancelApplyDialog dialog (withState applyChanges) unlock

  -- Set up store.
  props <- propertyList
  pconf <- mapM property =<< config "property-editor.conf" []
  mapM_ (storeAppend . (, Nothing)) $
    unionBy (eqBy propName) (catMaybes pconf) props

  store `on` rowDeleted $ const savePropertyEditorConfig

  onPropertyAdded   $ withState . addProperty
  onPropertyDeleted $ withState . removeProperty

  -- Set up buttons.
  widgetSetCanFocus prevB False
  prevB `onClicked` (withState prevTrack)

  widgetSetCanFocus nextB False
  nextB `onClicked` (withState nextTrack)

  widgetSetCanFocus ptrkB False
  cid <- ptrkB `onToggled` (withState togglePerTrack)

  -- Create an set up view.
  view <- treeViewNewWithModel store
  treeViewSetHeadersVisible view False
  treeViewSetRulesHint view True
  treeViewSetReorderable view True

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionBrowse

  name <- treeViewColumnNew
  treeViewAppendColumn view name
  nameCell <- cellRendererTextNew
  treeViewColumnPackStart name nameCell True
  cellLayoutSetAttributes name nameCell store $ \(p, _) ->
    [ cellText       := propName p
    , cellTextWeight := 800 ]

  text <- treeViewColumnNew
  treeViewAppendColumn view text
  textCell <- cellRendererTextNew
  treeViewColumnPackStart text textCell True
  cellLayoutSetAttributes text textCell store $ \(p, v) ->
    [ cellText         := maybe "" (showValue p) v
    , cellTextEditable := not (propReadOnly p) ]

  textCell `on` edited $ \[n] t -> do
   (p, old) <- storeGet n
   new      <- readOrNull p t
   when (isJust new && new /= old) $ do
     mapM_ (\(n, (p', _)) ->
             when (eqBy propKey p p') $ storeSet n (p', new)
           ) =<< storeToNumList
     enableApply True
     withState $ modify $ \state ->
       state { sCchgs = Map.insert (propKey p) (fromJust new) $ sCchgs state }

  -- Pack 'em all!
  upper <- dialogGetUpper dialog
  vbox  <- vBoxNew False 0
  containerSetBorderWidth vbox 7
  boxPackStartDefaults upper vbox

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  boxPackEndDefaults vbox scroll
  containerAdd scroll view

  bbox <- hButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  boxSetSpacing bbox 7
  boxPackStart vbox bbox PackNatural 7
  containerAdd bbox prevB
  containerAdd bbox nextB
  containerAdd bbox ptrkB

  widgetShowAll vbox

  return $ do
    tryLock $ do
      initState
      withState $ updateModel
      withSignalBlocked cid $ toggleButtonSetActive ptrkB True
      treeViewSetCursor view [0] $ Just (text, False)
      widgetGrabFocus view
    windowPresent dialog


readOrNull p t
  | null t    = return $ Just $ X.PropString ""
  | otherwise = readValue p t
