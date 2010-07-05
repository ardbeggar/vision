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

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad

import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

import Graphics.UI.Gtk

import UI
import Utils
import Compound
import Config
import Context
import Properties.Property
import Properties.Model


data Manager
  = Manager { pManager :: MVar (Maybe Dialog) }

initPropertyManager = do
  manager <- newMVar Nothing
  return $ augmentContext
    Manager { pManager = manager }

showPropertyManager =
  modifyMVar_ (pManager context) $ \maybeManager -> do
    manager <- case maybeManager of
      Just manager -> return manager
      Nothing      -> makePropertyManager
    windowSetTransientFor manager window
    windowPresent manager
    return $ Just manager

makePropertyManager = do
  dialog <- makeConfigDialog makePropertyManagerWidget
            getProperties setProperties
  windowSetTitle dialog "Manage properties"
  windowSetDefaultSize dialog 500 400
  dialog `onDestroy` do
    modifyMVar_ (pManager context) $ const $ return Nothing
  return dialog


data PM
  = PM { pStore   :: ListStore Property
       , pView    :: TreeView
       , pBox     :: VBox
       , pChanged :: IORef Bool
       }

instance CompoundWidget PM where
  type Outer PM = VBox
  outer = pBox

instance ConfigWidget PM where
  type Config PM = [Property]
  getConfig = listStoreToList . pStore
  setConfig pm config = do
    let store = pStore pm
    listStoreClear store
    mapM_ (listStoreAppend store) config
  getChanged = readIORef .  pChanged
  clearChanged pm = writeIORef (pChanged pm) False
  grabFocus = widgetGrabFocus . pView

makePropertyManagerWidget parent windowGroup onChanged = do
  store <- listStoreNewDND [] Nothing Nothing
  view  <- treeViewNewWithModel store
  treeViewSetRulesHint view True

  let showType p =
        case propType p of
          PropertyInt    -> "integer"
          PropertyString -> "string"
      showBool acc p =
        if acc p then "yes" else "no"
      addColumn title acc = do
        column <- treeViewColumnNew
        treeViewAppendColumn view column
        treeViewColumnSetTitle column title
        cell <- cellRendererTextNew
        treeViewColumnPackStart column cell True
        cellLayoutSetAttributes column cell store
          (\p -> [ cellText := acc p ])

  addColumn "Name"        propName
  addColumn "Key"         propKey
  addColumn "Type"        showType
  addColumn "Read-only" $ showBool propReadOnly
  addColumn "Custom"    $ showBool propCustom

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerAdd scroll view

  changed <- newIORef False

  let setChanged = do
        writeIORef changed True
        onChanged
      addProperty prop = do
        listStoreAppend store prop
        setChanged
        return ()

  ed   <- makePropertyEntryDialog windowGroup addProperty
  windowSetTransientFor (eDialog ed) parent
  parent `onDestroy` do widgetDestroy $ eDialog ed

  addB <- buttonNewFromStock stockAdd
  addB `onClicked` do eRun ed

  delB <- buttonNewFromStock stockRemove
  delB `onClicked` do
    (path, _) <- treeViewGetCursor view
    case path of
      [n] -> do
        prop <- listStoreGetValue store n
        when (propCustom prop) $ do
          listStoreRemove store n
          s <- listStoreGetSize store
          unless (s < 1) $
            treeViewSetCursor view [min (s - 1) n] Nothing
          setChanged
      _ ->
        return ()

  view `on` cursorChanged $ do
    (path, _) <- treeViewGetCursor view
    case path of
      [n] -> do
        prop <- listStoreGetValue store n
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

  return PM { pStore   = store
            , pView    = view
            , pBox     = vbox
            , pChanged = changed
            }


data ED =
  ED { eRun    :: IO ()
     , eDialog :: Dialog
     }

makePropertyEntryDialog windowGroup addProperty = do
  dialog <- dialogNew
  windowGroupAddWindow windowGroup dialog

  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk
  windowSetTransientFor dialog window
  windowSetModal dialog True

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

  upper <- dialogGetUpper dialog
  boxPackStartDefaults upper table
  widgetShowAll table

  let check = do
        name   <- trim <$> entryGetText nameE
        key    <- trim <$> entryGetText keyE
        exists <- isJust <$> property name
        dialogSetResponseSensitive dialog ResponseOk $
          not $ exists || null name || null key
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

  let run = do
        dialogSetDefaultResponse dialog ResponseOk
        dialogSetResponseSensitive dialog ResponseOk False
        entrySetText nameE ""
        entrySetText keyE ""
        comboSet typeC Nothing PropertyString
        comboSet roC Nothing False
        windowPresent dialog
        widgetGrabFocus nameE
        resp <- dialogRun dialog
        widgetHide dialog
        when (resp == ResponseOk) $ do
          pname  <- entryGetText nameE
          pkey   <- entryGetText keyE
          ptype  <- comboGet typeC
          pro    <- comboGet roC
          addProperty Property { propName      = trim pname
                               , propKey       = trim pkey
                               , propType      = ptype
                               , propReadOnly  = pro
                               , propCustom    = True
                               , propReadValue = Nothing
                               , propShowValue = Nothing
                               }

  return ED { eRun    = run
            , eDialog = dialog
            }


comboGet combo =
  toEnum <$> comboBoxGetActive combo

comboSet ::
  Enum a                     =>
  ComboBox                   ->
  Maybe (ConnectId ComboBox) ->
  a                          ->
  IO ()
comboSet combo maybeCid =
  wrap . comboBoxSetActive combo . fromEnum
  where wrap | Just cid <- maybeCid = withSignalBlocked cid
             | otherwise            = id

setupCombo combo ref =
  combo `on` changed $ (writeIORef ref =<< comboGet combo)
