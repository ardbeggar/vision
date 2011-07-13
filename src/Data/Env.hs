-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 13 Jul. 2010
--
--  Copyright (C) 2011 Oleg Belozeorov
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

{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FunctionalDependencies,
             UndecidableInstances #-}

module Data.Env
  ( mkEnv
  , (:*:)
  , extract
  , Extract (..)
  , build
  ) where


newtype Env ix a = Env a

mkEnv :: ix -> a -> Env ix a
mkEnv = const Env

data a :*: b = a :*: b
infixr 9 :*:


class EnvX ix a e | ix e -> a where
  extract :: ix -> e -> a

instance EnvX ix a ((Env ix a) :*: b) where
  extract _ ((Env a) :*: _) = a

instance EnvX ix a e => EnvX ix a (b :*: e) where
  extract ix (_ :*: e) = extract ix e


class EnvB ix a e r k | k -> ix a r where
  build :: e -> k -> r

instance EnvB ix a e (Env ix a :*: ()) (Env ix a) where
  build _ k = k :*: ()

instance (EnvB ix a e (r :*: ()) k, EnvB ix2 a2 e r2 k2) => EnvB ix a e (r :*: r2) (k :*: k2) where
  build e (a :*: b) =
    let k :*: _ = build e a
    in  k :*: (build e b)


data Extract ix a = Extract

instance EnvX ix a e => EnvB ix a e (Env ix a :*: ()) (Extract ix a) where
  build e d = (Env $ extract (ix d) e) :*: ()
    where ix :: Extract ix a -> ix
          ix = const undefined

