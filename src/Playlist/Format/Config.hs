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
  ) where

import Control.Monad

import Data.IORef

import System.IO.Unsafe

import Graphics.UI.Gtk

import Utils
import Config
import Compound
import Editor


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
  clearConfig  = const $ return ()
  getChanged   = fChanged
  clearChanged = fResetChanged
  grabFocus    = fFocus


makePlaylistFormatView parent onChanged = do
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

  editor <- unsafeInterleaveIO $ makeEditorDialog []
            makeFormatEditor $ \fe -> do
    let outerw = outer fe
    windowSetTitle outerw "Edit format"
    windowSetDefaultSize outerw 500 400

  let startEditing pos =
        runEditorDialog editor
        (case pos of
            Just pos -> listStoreGetValue store pos
            Nothing  -> return "")
        (\text -> case pos of
            Just pos -> do
              oldText <- listStoreGetValue store pos
              unless (text == oldText) $
                listStoreSetValue store pos text
            Nothing  -> do
              listStoreAppend store text
              return ())
        True parent

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


data FormatEditor
  = FormatEditor
    { eScroll :: ScrolledWindow
    , eBuff   :: TextBuffer
    , eView   :: TextView
    , eCid    :: ConnectId TextBuffer
    }

instance CompoundWidget FormatEditor where
  type Outer FormatEditor = ScrolledWindow
  outer = eScroll

instance EditorWidget FormatEditor where
  type Data FormatEditor = String
  setData       = formatEditorSetData
  getData       = formatEditorGetData
  clearData     = formatEditorClearData
  setupView     = formatEditorSetupView
  focusView     = formatEditorFocusView
  getState      = formatEditorGetState
  resetModified = formatEditorResetModified

formatEditorSetData =
  textBufferSetText . eBuff

formatEditorGetData FormatEditor { eBuff = buff } = do
  start <- textBufferGetStartIter buff
  end   <- textBufferGetEndIter buff
  textBufferGetText buff start end False

formatEditorClearData =
  flip textBufferSetText "" . eBuff

formatEditorSetupView FormatEditor { eBuff = buff } = do
  start <- textBufferGetStartIter buff
  textBufferPlaceCursor buff start

formatEditorFocusView =
  widgetGrabFocus . eView

formatEditorGetState =
  liftM (True, ) . textBufferGetModified . eBuff

formatEditorResetModified e =
  withSignalBlocked (eCid e) $ textBufferSetModified (eBuff e) False

makeFormatEditor _ onStateChanged = do
  buff <- textBufferNew Nothing
  cid <- buff `on` modifiedChanged $ onStateChanged

  view <- textViewNewWithBuffer buff

  scroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scroll ShadowIn
  containerSetBorderWidth scroll 7
  containerAdd scroll view

  return FormatEditor { eScroll = scroll
                      , eBuff   = buff
                      , eView   = view
                      , eCid    = cid
                      }
