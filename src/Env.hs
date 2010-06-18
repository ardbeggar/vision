-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 18 Jun. 2010
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

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             OverlappingInstances,
             TypeOperators #-}

module Env
  ( makeEnv
  , augmentEnv
  , getEnv
  ) where

infixr 9 :*
data a :* b = a :* b
data Nil = Nil

class Env e c where
  env :: c -> e

instance Env e (e :* b) where
  env (e :* _) = e

instance Env e b => Env e (a :* b) where
  env (_ :* b) = env b

makeEnv :: e -> e :* Nil
makeEnv e = e :* Nil

augmentEnv :: (?env :: e1) => e2 -> e2 :* e1
augmentEnv e = e :* ?env

getEnv = env ?env
