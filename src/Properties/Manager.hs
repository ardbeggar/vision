-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 5 Jul. 2010
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

module Properties.Manager
  ( initPropertyManager
  , showPropertyManager
  ) where

import Control.Applicative
import Control.Monad

import Data.IORef
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord
import Data.Char

import System.IO.Unsafe

import Graphics.UI.Gtk

import Utils
import Compound
import Context
import UI
import Editor
import Properties.Property
import Properties.Model


data Manager
  = Manager { pManager :: EditorDialog PropertyManager }

manager = pManager context


initPropertyManager = do
  manager <- unsafeInterleaveIO $ makeEditorDialog
             [(stockApply, ResponseApply)]
             makePropertyManager $ \m -> do
    let outerw = outer m
    windowSetTitle outerw "Manage properties"
    windowSetDefaultSize outerw 500 400
  return $ augmentContext
    Manager { pManager = manager }

showPropertyManager =
  runEditorDialog manager getProperties setProperties False window


data PropertyManager
  = PropertyManager { pStore   :: ListStore Property
                    , pView    :: TreeView
                    , pBox     :: VBox
                    , pChanged :: IORef Bool
                    , pNames   :: IORef (Set String)
                    }

instance CompoundWidget PropertyManager where
  type Outer PropertyManager = VBox
  outer = pBox

instance EditorWidget PropertyManager where
  type Data PropertyManager = [Property]
  getData       = propertyManagerGetData
  setData       = propertyManagerSetData
  clearData     = propertyManagerClearData
  setupView     = propertyManagerSetupView
  focusView     = propertyManagerFocusView
  getState      = propertyManagerGetState
  resetModified = propertyManagerResetModified

propertyManagerGetData =
  listStoreToList . pStore

propertyManagerSetData pm props = do
  let store = pStore pm
  listStoreClear store
  writeIORef (pNames pm) =<<
    foldM (\names prop -> do
              listStoreAppend store prop
              return $ Set.insert (propName prop) names
          ) Set.empty props

propertyManagerClearData pm = do
  listStoreClear $ pStore pm
  writeIORef (pNames pm) Set.empty

propertyManagerSetupView pm =
  treeViewSetCursor (pView pm) [0] Nothing

propertyManagerFocusView =
  widgetGrabFocus . pView

propertyManagerGetState =
  liftM (True, ) . readIORef . pChanged

propertyManagerResetModified =
  flip writeIORef False . pChanged

makePropertyManager parent notify = do
  names <- newIORef Set.empty

  store  <- listStoreNewDND [] Nothing Nothing
  sorted <- treeModelSortNewWithModel store

  let getByIter iter = do
        [n] <- treeModelGetPath store iter
        listStoreGetValue store n
      compBy f a b =
        comparing f <$> getByIter a <*> getByIter b

  treeSortableSetDefaultSortFunc sorted $ compBy (map toLower . propName)
  treeSortableSetSortFunc sorted 0 $ compBy (map toLower . propName)
  treeSortableSetSortFunc sorted 1 $ compBy (map toLower . propKey)
  treeSortableSetSortFunc sorted 2 $ compBy (fromEnum . propType)
  treeSortableSetSortFunc sorted 3 $ compBy (fromEnum . propReadOnly)
  treeSortableSetSortFunc sorted 4 $ compBy (fromEnum . propCustom)

  view <- treeViewNewWithModel sorted
  treeViewSetRulesHint view True

  let showType p =
        case propType p of
          PropertyInt    -> "integer"
          PropertyString -> "string"
      showBool acc p =
        if acc p then "yes" else "no"
      addColumn title id acc = do
        column <- treeViewColumnNew
        treeViewColumnSetSortColumnId column id
        treeViewAppendColumn view column
        treeViewColumnSetTitle column title
        cell <- cellRendererTextNew
        treeViewColumnPackStart column cell True
        cellLayoutSetAttributes column cell store
          (\p -> [ cellText := acc p ])

  addColumn "Name"      0   propName
  addColumn "Key"       1   propKey
  addColumn "Type"      2   showType
  addColumn "Read-only" 3 $ showBool propReadOnly
  addColumn "Custom"    4 $ showBool propCustom

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll view

  changed <- newIORef False

  let setChanged = do
        writeIORef changed True
        notify
      addProperty prop = do
        listStoreAppend store prop
        modifyIORef names $ Set.insert (propName prop)
        setChanged
        return ()
      exists name = Set.member name <$> readIORef names

  edlg <- unsafeInterleaveIO $ makeEditorDialog []
          (makePropertyEntry exists) (const $ return ())
  addB <- buttonNewFromStock stockAdd
  addB `onClicked`
    runEditorDialog edlg (return nullProperty) addProperty True parent

  delB <- buttonNewFromStock stockRemove
  delB `onClicked` do
    (path, _) <- treeViewGetCursor view
    case path of
      [n] -> do
        [n'] <- treeModelSortConvertPathToChildPath sorted path
        prop <- listStoreGetValue store n'
        when (propCustom prop) $ do
          listStoreRemove store n'
          modifyIORef names $ Set.delete (propName prop)
          s <- listStoreGetSize store
          unless (s < 1) $
            treeViewSetCursor view [min (s - 1) n] Nothing
          setChanged
      _ ->
        return ()

  view `on` cursorChanged $ do
    (path, _) <- treeViewGetCursor view
    case path of
      [_] -> do
        [n'] <- treeModelSortConvertPathToChildPath sorted path
        prop <- listStoreGetValue store n'
        widgetSetSensitive delB $ propCustom prop
      _ ->
        widgetSetSensitive delB False

  bbox <- hBoxNew True 5
  boxPackStartDefaults bbox addB
  boxPackStartDefaults bbox delB

  vbox <- vBoxNew False 5
  containerSetBorderWidth vbox 7
  boxPackStart vbox scroll PackGrow 0
  boxPackStart vbox bbox PackNatural 0

  return PropertyManager { pStore   = store
                         , pView    = view
                         , pBox     = vbox
                         , pChanged = changed
                         , pNames   = names
                         }


data PropertyEntry
  = PropertyEntry
    { eTable  :: Table
    , eName   :: Entry
    , eKey    :: Entry
    , eType   :: ComboBox
    , eRO     :: ComboBox
    , eExists :: String -> IO Bool
    }

instance CompoundWidget PropertyEntry where
  type Outer PropertyEntry = Table
  outer = eTable

instance EditorWidget PropertyEntry where
  type Data PropertyEntry = Property
  setData       = propertyEntrySetData
  getData       = propertyEntryGetData
  setupView     = propertyEntrySetupView
  getState      = propertyEntryGetState
  resetModified = const $ return ()

propertyEntrySetData e p = do
  entrySetText (eName e) (propName p)
  entrySetText (eKey e) (propKey p)
  comboSet (eType e) Nothing (propType p)
  comboSet (eRO e) Nothing (propReadOnly p)

propertyEntryGetData e = do
  pname  <- entryGetText $ eName e
  pkey   <- entryGetText $ eKey e
  ptype  <- comboGet $ eType e
  pro    <- comboGet $ eRO e
  return Property { propName      = trim pname
                  , propKey       = trim pkey
                  , propType      = ptype
                  , propReadOnly  = pro
                  , propCustom    = True
                  , propReadValue = Nothing
                  , propShowValue = Nothing
                  }

propertyEntrySetupView e =
  widgetGrabFocus $ eName e

propertyEntryGetState e = do
  name <- trim <$> entryGetText (eName e)
  key  <- trim <$> entryGetText (eKey e)
  (, True) <$> propertyEntryValid (eExists e) name key

propertyEntryValid exists name key = do
  e <- exists name
  return . not $ e || null name || null key

makePropertyEntry exists _ notify = do
  table <- tableNew 4 2 False
  containerSetBorderWidth table 7
  tableSetColSpacings table 15
  tableSetRowSpacings table 5

  let addPair m w b = do
        l <- labelNewWithMnemonic m
        miscSetAlignment l 0.0 0.5
        labelSetMnemonicWidget l w
        tableAttach table l 0 1 b (b + 1) [Fill] [] 0 0
        tableAttach table w 1 2 b (b + 1) [Expand, Fill] [] 0 0

  nameE <- entryNew
  addPair "_Name" nameE 0

  keyE <- entryNew
  addPair "_Key" keyE 1

  typeC <- comboBoxNewText
  comboBoxInsertText typeC (fromEnum PropertyString) "string"
  comboBoxInsertText typeC (fromEnum PropertyInt) "integer"
  typeRef <- newIORef PropertyString
  typeCid <- setupCombo typeC typeRef
  addPair "_Type" typeC 2

  roC <- comboBoxNewText
  comboBoxInsertText roC (fromEnum False) "no"
  comboBoxInsertText roC (fromEnum True) "yes"
  roRef <- newIORef False
  roCid <- setupCombo roC roRef
  addPair "_Read-only" roC 3

  let check = do
        notify
        key <- trim <$> entryGetText keyE
        case Map.lookup key builtinPropertyMap of
          Just prop -> do
            comboSet typeC (Just typeCid) $ propType prop
            widgetSetSensitive typeC False
            comboSet roC (Just roCid) $ propReadOnly prop
            widgetSetSensitive roC False
          Nothing   -> do
            comboSet typeC (Just typeCid) =<< readIORef typeRef
            widgetSetSensitive typeC True
            comboSet roC (Just roCid) =<< readIORef roRef
            widgetSetSensitive roC True

      checkInsert str pos = do
        check
        return $ (length str) + pos

      checkDelete _ _ = check

  nameE `afterInsertText` checkInsert
  nameE `afterDeleteText` checkDelete
  keyE  `afterInsertText` checkInsert
  keyE  `afterDeleteText` checkDelete

  return PropertyEntry { eTable  = table
                       , eName   = nameE
                       , eKey    = keyE
                       , eType   = typeC
                       , eRO     = roC
                       , eExists = exists
                       }


nullProperty =
  Property { propName      = ""
           , propKey       = ""
           , propType      = PropertyString
           , propReadOnly  = False
           , propCustom    = True
           , propReadValue = Nothing
           , propShowValue = Nothing
           }


comboGet combo =
  toEnum <$> comboBoxGetActive combo

comboSet ::
  Enum a                     =>
  ComboBox                   ->
  Maybe (ConnectId ComboBox) ->
  a                          ->
  IO ()
comboSet combo cid =
  maybe id withSignalBlocked cid . comboBoxSetActive combo . fromEnum

setupCombo combo ref =
  combo `on` changed $ (writeIORef ref =<< comboGet combo)
