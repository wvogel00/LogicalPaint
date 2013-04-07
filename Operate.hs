module Operate where

import Interface
import Data.Array.IO

--read the value on the position, (i,j).
readMap :: Int -> Int -> Map -> IO Bool
readMap i j mp = do
	lmap' <- readArray (lmap mp) j
	return =<< readArray lmap' i
