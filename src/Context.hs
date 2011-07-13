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
             FlexibleContexts,
             OverlappingInstances,
             TypeOperators,
             DeriveDataTypeable,
             UndecidableInstances,
             ImplicitParams,
             StandaloneDeriving #-}

module Context
  ( ContextClass
  , ContextType
  , makeContext
  , augmentContext
  , context
  ) where


infixr 9 :*
data a :* b = a :* b
data Nil = Nil

class ContextClass c a where
  ctxt :: a -> c

instance ContextClass c (c :* b) where
  ctxt (c :* _) = c

instance ContextClass c b => ContextClass c (a :* b) where
  ctxt (_ :* b) = ctxt b


type ContextType a b = a :* b


makeContext :: c -> ContextType c Nil
makeContext c = c :* Nil

augmentContext :: (?context :: c1) => c2 -> ContextType c2 c1
augmentContext c = c :* ?context

context :: (?context :: a, ContextClass c a) => c
context = ctxt ?context
