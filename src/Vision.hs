-- -*-haskell-*-
--  Vision (for the Voice): an XMMS2 client.
--
--  Author:  Oleg Belozeorov
--  Created: 11 Jun. 2010
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

module Main
  where

import Handler


main = do
  hh <- mkHandler
  let onHH = modifyHandler hh

  i1 <- AddHandler `onHH` (putStrLn . ("1 " ++))
  i2 <- AddHandler `onHH` (putStrLn . ("2 " ++))

  invoke hh "zopa"

  modifyHandler hh RemoveHandler i1

  invoke hh "pesda"

  modifyHandler hh RemoveHandler i2

  invoke hh "khooy"

  return ()