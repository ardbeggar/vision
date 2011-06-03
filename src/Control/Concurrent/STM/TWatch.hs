module Control.Concurrent.STM.TWatch
  ( TWatch
  , newTWatch
  , newEmptyTWatch
  , watch
  ) where

import Control.Concurrent.STM
import Control.Monad


data Eq a => TWatch a =
  TWatch (TVar a) (TVar (Maybe a))

newTWatch :: Eq a => TVar a -> a -> STM (TWatch a)
newTWatch src = newTWatch' src . Just

newEmptyTWatch :: Eq a => TVar a -> STM (TWatch a)
newEmptyTWatch src = newTWatch' src Nothing

newTWatch' :: Eq a => TVar a -> Maybe a -> STM (TWatch a)
newTWatch' src = liftM (TWatch src) . newTVar

watch :: Eq a => TWatch a -> STM a
watch (TWatch src cur) = do
  srcv <- readTVar src
  curv <- readTVar cur
  case curv of
    Just v | v == srcv -> retry
    _                  -> do
      writeTVar cur $ Just srcv
      return srcv
