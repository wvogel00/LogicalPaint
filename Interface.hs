module Interface where

import Data.Array.IO

data Map = Map{
	lmap :: IOArray Int (IOArray Int Bool),
	size :: (Int,Int),
	vlines :: [VLine],
	hlines :: [HLine]
	}

instance Show Map where
	show mp = show (fst $ size mp) ++ "," ++ show (snd $ size mp)

type Size = (Int,Int)
type Line = [Int]
type VLine = [Int]
type HLine = [Int]
