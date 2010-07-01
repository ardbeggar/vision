-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Jul. 2010
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

module Playlist.Format.Config
  ( makePlaylistFormatView
  , ConfigWidget (..)
  ) where

import Control.Monad

import Data.IORef

import Graphics.UI.Gtk

import Utils
import UI
import Compound


class ConfigWidget cw where
  type Config cw
  getConfig    :: cw -> IO (Config cw)
  setConfig    :: cw -> Config cw -> IO ()
  getChanged   :: cw -> IO Bool
  clearChanged :: cw -> IO ()
  grabFocus    :: cw -> IO ()


data FormatView
  = FormatView { fBox          :: HBox
               , fSet          :: [String] -> IO ()
               , fGet          :: IO [String]
               , fChanged      :: IO Bool
               , fResetChanged :: IO ()
               , fFocus        :: IO ()
               }

instance CompoundWidget FormatView where
  type Outer FormatView = HBox
  outer = fBox

instance ConfigWidget FormatView where
  type Config FormatView = [String]
  getConfig    = fGet
  setConfig    = fSet
  getChanged   = fChanged
  clearChanged = fResetChanged
  grabFocus    = fFocus


makePlaylistFormatView windowGroup onChanged = do
  box <- hBoxNew False 5
  containerSetBorderWidth box 7

  bbox <- vButtonBoxNew
  buttonBoxSetLayout bbox ButtonboxStart
  boxSetSpacing bbox 5
  boxPackEnd box bbox PackNatural 0

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  boxPackStartDefaults box scroll

  let mkButton m = do
        button <- buttonNewWithMnemonic m
        widgetSetCanFocus button False
        containerAdd bbox button
        return button

  addB    <- mkButton "_Add"
  editB   <- mkButton "_Edit"
  deleteB <- mkButton "_Delete"

  changedRef <- newIORef False
  store      <- listStoreNew []

  view  <- treeViewNewWithModel store
  treeViewSetRulesHint view True
  treeViewSetHeadersVisible view False
  treeViewSetReorderable view True
  containerAdd scroll view

  column <- treeViewColumnNew
  treeViewAppendColumn view column

  cell <- cellRendererTextNew
  treeViewColumnPackStart column cell True
  cellLayoutSetAttributes column cell store $ \t -> [ cellText := t ]

  sel <- treeViewGetSelection view
  treeSelectionSetMode sel SelectionBrowse
  sel `onSelectionChanged` do
    n <- treeSelectionCountSelectedRows sel
    let enable = n > 0
    widgetSetSensitive editB enable
    widgetSetSensitive deleteB enable

  let handleChange path _ = do
        treeViewSetCursor view path Nothing
        handleChange'
      handleChange' = do
        writeIORef changedRef True
        onChanged

  insId <- store `on` rowInserted $ handleChange
  chgId <- store `on` rowChanged  $ handleChange
  delId <- store `on` rowDeleted  $ \[n] -> do
    size <- listStoreGetSize store
    case size of
      0 -> return ()
      k -> treeViewSetCursor view [if k > n then n else k - 1] Nothing
    handleChange'

  let withoutChanged f = do
        mapM_ signalBlock [insId, chgId, delId]
        f
        mapM_ signalUnblock [insId, chgId, delId]

  editPosRef <- newIORef Nothing
  editor     <- makeFormatEditor windowGroup $ \text ->
    fmaybeM_ text $ \text -> do
      editPos <- readIORef editPosRef
      case editPos of
        Just pos -> do
          oldText <- listStoreGetValue store pos
          unless (text == oldText) $
            listStoreSetValue store pos text
        Nothing  -> do
          listStoreAppend store text
          return ()

  let startEditing pos = do
        writeIORef editPosRef pos
        editor =<< maybe (return "") (listStoreGetValue store) pos

  view `onRowActivated` \[n] _ -> startEditing $ Just n
  addB    `onClicked` startEditing Nothing
  editB   `onClicked` do
    ([n], _) <- treeViewGetCursor view
    startEditing $ Just n
  deleteB `onClicked` do
    ([n], _) <- treeViewGetCursor view
    listStoreRemove store n

  let get = listStoreToList store
      set items = do
        withoutChanged $ mapM_ (listStoreAppend store) items
        treeViewSetCursor view [0] Nothing
      changed = readIORef changedRef
      resetChanged = writeIORef changedRef False
      focus = widgetGrabFocus view

  widgetShowAll box
  return FormatView { fBox     = box
                    , fGet     = get
                    , fSet     = set
                    , fChanged = changed
                    , fResetChanged = resetChanged
                    , fFocus   = focus
                    }

makeFormatEditor windowGroup onDone = do
  dialog <- dialogNew
  windowGroupAddWindow windowGroup dialog
  windowSetTransientFor dialog window
  windowSetModal dialog True
  windowSetTitle dialog "Edit format"
  windowSetDefaultSize dialog 500 400
  dialogSetHasSeparator dialog False
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  dialogAddButtonCR dialog "gtk-ok" ResponseOk

  upper <- dialogGetUpper dialog
  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerSetBorderWidth scroll 7
  containerAdd upper scroll

  buff <- textBufferNew Nothing
  view <- textViewNewWithBuffer buff
  containerAdd scroll view

  return $ \text -> do
    textBufferSetText buff text
    textBufferSetModified buff False
    dialogSetDefaultResponse dialog ResponseOk
    widgetShowAll dialog
    widgetGrabFocus view
    resp <- dialogRun dialog
    widgetHide dialog
    onDone =<< case resp of
      ResponseOk -> do
        modified <- textBufferGetModified buff
        if modified
          then do
          start <- textBufferGetStartIter buff
          end   <- textBufferGetEndIter buff
          text  <- textBufferGetText buff start end False
          return $ Just $ trim text
          else
          return Nothing
      _ ->
        return Nothing
