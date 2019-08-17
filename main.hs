{-# OPTIONS_GHC -fno-warn-tabs #-}

import Prelude
import LIB.CSV

main = do
	putStrLn $ "test1"		++ "\n-------------------"
	t1 <- loadCSV "TEST/tester.csv"
	putStrLn t1
	putStrLn "\n"