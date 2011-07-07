-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 17 Jun. 2010
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

module Utils
  ( encodeURL
  , decodeURL
  , trd
  , trim
  , bracket_
  , catchResult
  , setupTreeViewPopup
  , dialogAddButtonCR
  , withJust
  , withSignalBlocked
  , hideOnDeleteEvent
  , eqBy
  , tryModifyMVar_
  , EntryIcon (..)
  , onIconPress
  , secondaryIconSensitive
  ) where

import Prelude hiding (catch)
import Control.Monad.CatchIO hiding (Handler)

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.MVar
import Data.Char hiding (Control)
import Codec.Binary.UTF8.String

import Foreign.C.Types

import Graphics.UI.Gtk
import System.Glib.Properties

import XMMS2.Client


encodeURL = encodeURL' . encodeString
encodeURL' [] = []
encodeURL' (' ':cs) = '+' : encodeURL' cs
encodeURL' (c:cs)
  | goodChar c =
    c : encodeURL' cs
  | otherwise  =
    let code = ord c
        c1   = intToDigit $ code `div` 16
        c2   = intToDigit $ code `rem` 16 in
    '%' : c1 : c2 : encodeURL' cs

goodChar ':' = True
goodChar '/' = True
goodChar '-' = True
goodChar '.' = True
goodChar '_' = True
goodChar c   = isAsciiUpper c || isAsciiLower c || isDigit c

decodeURL = decodeString . decodeURL'
decodeURL' []         = []
decodeURL' ('+' : cs) = ' ' : decodeURL' cs
decodeURL' ('%' : cs) = let (c, cs') = decodeByte cs in c : decodeURL' cs'
decodeURL' (c : cs)   = c : decodeURL' cs

decodeByte (d1 : d0 : cs)
  | isHexDigit d1 && isHexDigit d0 = (chr $ digitToInt d1 * 16 + digitToInt d0, cs)
decodeByte _ = error "invalid URL"

trd (_, _, c) = c

trim = f . f where f = reverse . dropWhile isSpace

catchResult def conv =
  (conv <$> result) `catch` \(_ :: XMMSException) -> return def

setupTreeViewPopup view popup = do
  view `on` popupMenuSignal $ (menuPopup popup Nothing >> return True)

  sel     <- treeViewGetSelection view
  selMode <- treeSelectionGetMode sel
  let cond =
        case selMode of
          SelectionMultiple ->
            (2 >) <$> liftIO (treeSelectionCountSelectedRows sel)
          SelectionNone     ->
            return False
          _                 ->
            return True
      setCursor = do
        doIt <- cond
        when doIt $ do
          (x, y)    <- eventCoordinates
          maybePath <- liftIO $ treeViewGetPathAtPos view (round x, round y)
          case maybePath of
            Just (path, _, _) ->
              liftIO $ treeViewSetCursor view path Nothing
            Nothing           ->
              return ()

  view `on` buttonPressEvent $ tryEvent $ do
    RightButton <- eventButton
    SingleClick <- eventClick
    stamp       <- eventTime
    setCursor
    liftIO $ widgetGrabFocus view
    liftIO $ menuPopup popup $ Just (RightButton, stamp)

dialogAddButtonCR dialog label response = do
  button <- dialogAddButton dialog label response
  dialog `on` keyPressEvent $ tryEvent $ do
    "Return"  <- eventKeyName
    [Control] <- eventModifier
    liftIO $ do
      doIt <- button `get` widgetSensitive
      when doIt $ do
        widgetActivate button
        return ()

fmaybe :: b -> Maybe a -> (a -> b) -> b
fmaybe = flip . maybe

withJust :: Monad m => Maybe a -> (a -> m b) -> m ()
withJust m f = fmaybe (return ()) m $ \a -> f a >> return ()

withSignalBlocked s f =
  block $ do
    signalBlock s
    r <- unblock f `onException` signalUnblock s
    signalUnblock s
    return r

hideOnDeleteEvent window =
  window `on` deleteEvent $ do
    liftIO $ widgetHide window
    return True

eqBy f a b = f a == f b

{-# INLINE tryModifyMVar_ #-}
tryModifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
tryModifyMVar_ m io =
  block $ do
    maybeA <- tryTakeMVar m
    case maybeA of
      Just a  -> do
        a' <- unblock (io a) `onException` putMVar m a
        putMVar m a'
      Nothing ->
        return ()


deriving instance MonadCatchIO (ResultM c a)


data EntryIcon
  = PrimaryIcon
  | SecondaryIcon
    deriving (Enum, Eq)

onIconPress
  :: EntryClass ec
  => ec
  -> (EntryIcon -> IO ())
  -> IO (ConnectId ec)
onIconPress entry handler =
  connectGeneric "icon-press" False entry $ \_ pos _ ->
    handler $ toEnum $ fromIntegral (pos :: CInt)

secondaryIconSensitive =
  newAttrFromBoolProperty "secondary-icon-sensitive"
