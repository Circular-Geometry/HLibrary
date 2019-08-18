{-# OPTIONS_GHC -fno-warn-tabs #-}

-----------------------------------------------------------------------------
-- 
-- Module      :  LIB.CSV
-- Copyright   :  (c) Lloyd Cope 2019
-- License     :  Apache 2.0 (see LICENSE)
--
-- For reading and writing CSV files
--
-----------------------------------------------------------------------------

module LIB.CSV where

import System.FilePath
import Data.List


readCSV :: FilePath -> IO [[String]]
readCSV path = do
	f <- readFile path
	return $ csv_start path f
	where	
		csv_start file str = csv file (1,1) (1,1) False str [] [] []

		csv file (ln,col) (ln',col') escaped str record line ret =
			case ((ln == ln') && (col == col'), escaped, str) 
			  of
			     (_    , False, '\n' : xs)		-> csv file (ln+1,1)   (ln+1,1)   False   xs []                []                (ret ++ [line ++ [record]])
			     (_    , True , '\n' : xs)		-> csv file (ln+1,1)   (ln',col') True    xs (record ++ "\n")  line               ret
			     (_    , False, ','  : xs)		-> csv file (ln,col+1) (ln,col+1) False   xs []               (line ++ [record])  ret
			     (_    , True , ','  : xs)		-> csv file (ln,col+1) (ln',col') True    xs (record ++ ",")   line               ret
			     (True , False, '\"' : xs)  	-> csv file (ln,col+1) (ln',col') True    xs record            line               ret
			     (False, True , '\"' : '\"' : xs)	-> csv file (ln,col+2) (ln',col') True    xs (record ++ "\"")  line               ret
			     (False, True , '\"' : ','  : xs)	-> csv file (ln,col+2) (ln,col+2) False   xs []               (line ++ [record])  ret
			     (False, True , '\"' : '\n' : xs)	-> csv file (ln+1,1)   (ln+1,1)   False   xs []               []                 (ret ++ [line ++ [record]])
			     (False, True , '\"' : [])		->                                                                               (ret ++ [line ++ [record]])
			     (False, False, '\"' : xs)		-> error $ "\nloadCSV has had a syntax issue\n" ++
			     						   "unexpected quotation mark\n" ++
			     						   "file   : " ++ file ++ "\n" ++
									   "line   : " ++ (show $ ln ) ++ "\n" ++
									   "column : " ++ (show $ col) ++ "\n" ++
									   "(ln',col'):" ++ (show $ (ln',col'))
			     (_    , _    , x    : xs)		-> csv file (ln,col+1) (ln',col') escaped xs (record ++ [x])   line               ret
			     (_    , False,        [])		->                                                                               (ret ++ [line ++ [record]])
			     (_    , True ,        [])		-> error $ "\nloadCSV has had a syntax issue\n" ++
			     						   "quote was expected at end of file\n" ++
			     						   "file   : " ++ file ++ "\n"
			



writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV path contents = do
	str <- return $ concatLines $ map concatRecords contents
	writeFile path str
	where	
		concatLines (x:[])	= x
		concatLines (x:xs)	= x ++ "\n" ++ (concatLines xs)
		concatLines []		= []
		
		concatRecords (x:[])	= escape x
		concatRecords (x:xs)	= (escape x) ++ "," ++ (concatRecords xs)
		concatRecords []	= []
		
		escape str
			| '"'  `elem` str	= toEscape str
			| ','  `elem` str	= toEscape str
			| '\n' `elem` str	= toEscape str
			| otherwise		= str

		toEscape str = "\"" ++ (toEscape' str) ++ "\""
		
		toEscape' ('\"' : xs)	= '\"' : '\"' : toEscape' xs
		toEscape' ( x   : xs)	= x           : toEscape' xs
		toEscape' []		= []



-- Notes
-------------------------------------------------------------------------------
-- The format for CSV can be located at https://tools.ietf.org/html/rfc4180
--
-- A better implementation (don't know, but probably) can be found here
-- http://hackage.haskell.org/package/cassava-0.5.1.0/docs/Data-Csv.html
