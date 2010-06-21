-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 4 Mar. 2009
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

module Properties.Manager
  ( setupManager
  ) where

import Control.Applicative
import Control.Monad

import Data.IORef
import qualified Data.Map as Map

import Graphics.UI.Gtk

import UI
import Utils

import Properties.Property
import Properties.Model


setupManager = do
  pmdlg  <- makePropertyManagerDialog

  group <- actionGroupNew "properties"
  insertActionGroup group 1

  actionGroupAddActions group
    [ ActionEntry { actionEntryName        = "manage"
                  , actionEntryLabel       = "_Manage properties…"
                  , actionEntryStockId     = Just "gtk-preferences"
                  , actionEntryAccelerator = Just ""
                  , actionEntryTooltip     = Nothing
                  , actionEntryCallback    = windowPresent pmdlg } ]

  return ()


makePropertyManagerDialog = do
  windowGroup <- windowGroupNew
  let ?windowGroup = windowGroup

  dialog <- dialogNew
  windowGroupAddWindow ?windowGroup dialog
  fixWindow dialog
  windowSetTransientFor dialog mainWindow
  windowSetModal dialog False
  windowSetTitle dialog "Manage properties"
  windowSetDefaultSize dialog 500 400
  dialogSetHasSeparator dialog False

  dialogAddButtonCR dialog "gtk-close" ResponseClose
  dialogSetDefaultResponse dialog ResponseClose
  dialog `onResponse` \_ -> do
    widgetHide dialog
    dialogSetDefaultResponse dialog ResponseClose

  manager <- makePropertyManager
  upper   <- dialogGetUpper dialog
  boxPackStartDefaults upper manager

  return dialog


makePropertyManager = do
  view <- treeViewNewWithModel propertyStore
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
        cellLayoutSetAttributes column cell propertyStore
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

  run  <- makeRunPropertyEntryDialog
  addB <- buttonNewFromStock stockAdd
  addB `onClicked` run

  delB <- buttonNewFromStock stockRemove
  delB `onClicked` do
    (path, _) <- treeViewGetCursor view
    case path of
      [n] -> do
        prop <- listStoreGetValue propertyStore n
        when (propCustom prop) $ delProperty prop
        s <- listStoreGetSize propertyStore
        unless (s < 1) $
          treeViewSetCursor view [min (s - 1) n] Nothing
      _ ->
        return ()

  view `on` cursorChanged $ do
    (path, _) <- treeViewGetCursor view
    case path of
      [n] -> do
        prop <- listStoreGetValue propertyStore n
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

  widgetShowAll vbox
  return vbox


makeRunPropertyEntryDialog = do
  dialog <- dialogNew
  windowGroupAddWindow ?windowGroup dialog
  fixWindow dialog
  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk
  windowSetTransientFor dialog mainWindow
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
        exists <- Map.member name <$> readIORef propertyMap
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

  return $ do
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
                           , propShowValue = Nothing }


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
