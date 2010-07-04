-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 1 Jul. 2010
--
--  Copyright (C) 2009-2010 Oleg Belozeorov
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

module Location.History
  ( History
  , initHistory
  , makeHistoryCompletion
  , addToHistory
  ) where

import Data.List
import Data.Char

import Graphics.UI.Gtk

import Config
import Context


data History
  = History { hStore :: ListStore String }

historyStore = hStore context


initHistory = do
  history <- config "history.urls" []
  store   <- listStoreNewDND history Nothing Nothing
  return $ augmentContext
    History { hStore = store }

makeHistoryCompletion = do
  comp <- entryCompletionNew

  entryCompletionSetModel comp $ Just historyStore
  entryCompletionSetTextModel comp historyStore
  entryCompletionSetMatchFunc comp $ \s i -> do
    [n] <- treeModelGetPath historyStore i
    t   <- listStoreGetValue historyStore n
    return $ isInfixOf s $ map toLower t

  entryCompletionSetPopupCompletion comp True
  entryCompletionSetPopupSingleMatch comp True

  return comp

addToHistory url = do
  history <- listStoreToList historyStore
  maybe (return ()) (listStoreRemove historyStore) $ elemIndex url history
  listStorePrepend historyStore url
  writeConfig "history.urls" =<< listStoreToList historyStore
  return ()
