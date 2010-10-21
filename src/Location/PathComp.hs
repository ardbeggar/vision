-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 21 Sep. 2010
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

module Location.PathComp
  ( PathComp
  , makePathComp
  , pathComp
  , updatePathComp
  , clearPathComp
  ) where

import Control.Monad
import Control.Monad.Trans

import Prelude hiding (catch)
import Control.Monad.CatchIO

import Data.IORef

import System.FilePath

import Graphics.UI.Gtk

import XMMS2.Client

import XMMS
import Utils


data PathComp
  = PathComp
    { pathComp :: EntryCompletion
    , pStore   :: ListStore String
    , pURL     :: IORef String
    }


makePathComp = do
  comp  <- entryCompletionNew
  store <- listStoreNewDND [] Nothing Nothing
  url   <- newIORef ""

  entryCompletionSetModel comp $ Just store
  entryCompletionSetTextModel comp store

  entryCompletionSetPopupCompletion comp True
  entryCompletionSetPopupSingleMatch comp False
  entryCompletionSetInlineCompletion comp True

  return PathComp { pathComp = comp
                  , pStore   = store
                  , pURL     = url
                  }

updatePathComp pc url = do
  let (hd, _) = splitFileName  url
  old <- readIORef $ pURL pc
  unless (old == hd) $ do
    writeIORef (pURL pc) hd
    xformMediaBrowse xmms hd >>* do
      (flip catch) (\(_ :: XMMSException) -> return ()) $ do
        r <- result
        liftIO $ do
          listStoreClear $ pStore pc
          forM_ r $ \e ->
            when (entryIsDir e) $ do
              listStoreAppend (pStore pc) . decodeURL $ entryPath e
              return ()
          entryCompletionComplete $ pathComp pc

clearPathComp pc = do
  listStoreClear $ pStore pc
  writeIORef (pURL pc) ""

