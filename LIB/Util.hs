{-# OPTIONS_GHC -fno-warn-tabs #-}

-----------------------------------------------------------------------------
-- 
-- Module      :  LIB.Util
-- Copyright   :  (c) Lloyd Cope 2019
-- License     :  Apache 2.0 (see LICENSE)
--
-- Random Stuff that I find I have to use repeatedly
--
-----------------------------------------------------------------------------

module LIB.Util where

import Data.List

split :: Eq a => [a] -> [a] -> [[a]]
split _         []			= [[]]
split dilimiter lst
	| l < n				= [lst]
	| dilimiter `isPrefixOf` lst	= [] : ret'
	| otherwise			= (x : rh) : rt
	where	x	= head lst
		rh	= head ret
		rt	= tail ret
		ret	= split dilimiter $ tail lst
		ret'	= split dilimiter $ drop n lst
		n	= length dilimiter
		l	= length lst