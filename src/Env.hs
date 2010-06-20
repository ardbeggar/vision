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
  ( EnvClass
  , EnvType
  , makeEnv
  , augmentEnv
  , getEnv
  ) where


infixr 9 :*
data a :* b = a :* b
data Nil = Nil

class EnvClass e c where
  env :: c -> e

instance EnvClass e (e :* b) where
  env (e :* _) = e

instance EnvClass e b => EnvClass e (a :* b) where
  env (_ :* b) = env b


type EnvType a b = a :* b


makeEnv :: e -> EnvType e Nil
makeEnv e = e :* Nil

augmentEnv :: (?env :: e1) => e2 -> EnvType e2 e1
augmentEnv e = e :* ?env

getEnv :: (?env :: c, EnvClass e c) => e
getEnv = env ?env
