-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 6 Jul. 2010
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

module Properties.Editor.Model
  ( initEditorModel
  , store
  , populateModel
  , resetModel
  , propertyText
  , getNavEnables
  , prevTrack
  , nextTrack
  , togglePerTrack
  , changeProperty
  , writeProperties
  ) where

import Prelude hiding (lookup)

import Control.Concurrent.MVar
import Control.Monad.State hiding (State, withState, state)
import Control.Applicative
import Control.Arrow

import Data.Maybe
import Data.List hiding (lookup, union)
import Data.Array
import Data.Map (Map, union)
import qualified Data.Map as Map

import Graphics.UI.Gtk hiding (add, get, Entry)

import qualified XMMS2.Client as X

import XMMS
import Context
import Config
import Utils
import Handler
import Medialib
import Properties.Property
import Properties.Model


type Entry = (MediaInfo, MediaInfo)

data State
  = State { sPos      :: Int
          , sSize     :: Int
          , sIds      :: Array Int X.MediaId
          , sEntries  :: Map X.MediaId Entry
          , sCurrent  :: Entry
          , sPerTrack :: Bool
          }

makeState list =
  let size    = length list
      ids     = array (0, size - 1) $ zip [0 .. ] $ map fst list
      entries = Map.fromList $
        map (\(id, info) -> (id, (info, Map.empty))) list
      current = fromJust $ Map.lookup (ids ! 0) entries
  in State { sPos      = 0
           , sSize     = size
           , sIds      = ids
           , sEntries  = entries
           , sCurrent  = current
           , sPerTrack = True
           }

data Model
  = Model { mState :: MVar (Maybe State)
          , mStore :: ListStore Property
          }

state = mState context
store = mStore context

setupState list =
  modifyMVar_ state . const . return . Just $ makeState list

resetState =
  modifyMVar_ state . const $ return Nothing

withState d f =
  modifyMVar state $ \state ->
    case state of
      Just s  -> do
        (a, s') <- runStateT f s
        return (Just s', a)
      Nothing ->
        return (Nothing, d)

initEditorModel = do
  context <- initContext
  let ?context = context

  cid <- store `on` rowDeleted $ const saveConfig
  onProperties . add . ever . const $
    withSignalBlocked cid updateProperties

  onServerConnection . add . ever $ \conn ->
    unless conn $ do
      resetModel
      touchAll

  loadConfig

  return ?context

initContext = do
  state <- newMVar Nothing
  store <- listStoreNew []
  return $ augmentContext
    Model { mState = state
          , mStore = store
          }

loadConfig = do
  cfg <- mapM property =<< config configFile []
  populateStore cfg

saveConfig = do
  names <- map propName <$> listStoreToList store
  writeConfig configFile names
  return ()

updateProperties = do
  cur <- mapM (property . propName) =<< listStoreToList store
  listStoreClear store
  populateStore cur

populateStore cur = do
  all <- getProperties
  mapM_ (listStoreAppend store) $
    unionBy (eqBy propName) (catMaybes cur) all

configFile = "property-editor.conf"

populateModel list = do
  setupState list
  touchAll

resetModel =
  resetState

propertyText prop = withState "" $ do
  (b, c) <- gets sCurrent
  let key = propKey prop
      val = Map.lookup key c `mplus` Map.lookup key b
  return $ maybe "" (showValue prop) val

getNavEnables = withState (False, False) $ do
  t <- gets sPerTrack
  p <- gets sPos
  s <- gets sSize
  return (t && p > 0, t && p < s - 1)

prevTrack = stepTrack (-1)
nextTrack = stepTrack 1

stepTrack inc = do
  withState () $ modify $ \s ->
    let e = Map.insert (sIds s ! sPos s) (sCurrent s) (sEntries s)
        p = sPos s + inc
        c = e Map.! (sIds s ! p) in
    s { sEntries = e
      , sPos     = p
      , sCurrent = c
      }
  touchAll

touchAll = do
  s <- listStoreGetSize store
  mapM_ touch [0 .. s - 1]

touch n = do
  Just iter <- treeModelGetIter store [n]
  treeModelRowChanged store [n] iter

togglePerTrack = do
  withState () $ modify $ \s ->
    if sPerTrack s
    then let
      e = Map.insert (sIds s ! sPos s) (sCurrent s) (sEntries s)
      c = (common $ Map.elems e, Map.empty)
      in s { sPerTrack = False
           , sEntries  = e
           , sCurrent  = c
           }
    else let
      u = snd $ sCurrent s
      e = Map.map (second $ Map.union u) $ sEntries s
      c = e Map.! (sIds s ! sPos s)
      in s { sPerTrack = True
           , sEntries  = e
           , sCurrent  = c
           }
  touchAll

common e =
  let (c, i)     = foldl g (h, h) t
      f a b      = if a == b then Just a else Nothing
      g (c, i) e = (Map.differenceWith f c e, Map.intersection i e)
      (h : t)    = map (uncurry $ flip Map.union) e
  in Map.intersection c i

changeProperty n prop text = do
  maybeVal <- case trim text of
    [] -> return . Just $ X.PropString ""
    st -> readValue prop st
  case maybeVal of
    Nothing  -> return False
    Just val -> withState False $ do
      (b, c) <- gets sCurrent
      let key = propKey prop
          cur = Map.lookup key c `mplus` Map.lookup key b
          res = maybeVal /= cur
      when res $ do
        modify $ \s -> s { sCurrent = (b, Map.insert key val c) }
        liftIO $ touch n
      return res

writeProperties = do
  changes <- withState [] extractChanges
  mapM_ writeForId changes
  where writeForId (id, cs) =
          mapM_ (writeProperty id) cs

writeProperty id (key, val) =
  X.medialibEntryPropertySet xmms id
  (Just "client/generic/override")
  key val

extractChanges = do
  s <- get
  let c = sCurrent s
      e = if sPerTrack s
          then Map.insert (sIds s ! sPos s) c (sEntries s)
          else let u = snd c in
          Map.map (second $ union u) $ sEntries s
      e' = Map.map (\(b, c) -> (c `union` b, Map.empty)) e
  put s { sEntries = e'
        , sCurrent = (snd c `union` fst c, Map.empty)
        }
  return $ map (\(id, (_, c)) -> (id, Map.toList c)) $ Map.toList e


