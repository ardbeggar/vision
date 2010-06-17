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
import Control.Concurrent.MVar

main = do
  h <- newMVar make
  let onH = modifyMVar h
  id <- onH . add . ever $ print
  onH . add . ever $ putStrLn  . ("zopa: " ++) . show
  onH $ invoke 10
  onH $ invoke 20
  onH $ invoke 30
  onH $ remove id
  onH $ invoke 10
  onH . add . once $ \v -> do
    putStr "pesda: "
    print v
  onH . add . ever . const $ print "here we are"
  onH $ invoke 20
  onH $ invoke 30
  return ()
