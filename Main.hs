import Interface
import FileParser
import Viewer
import Control.Applicative ((<$>))
import Data.Array.IO (readArray,newArray,IOArray(..))
import qualified Control.Exception as E
main = do
	print "input the file name which you want to solve.."
	args <- getLine
	catch (readFile args >>= mkInitMap >>= showPaint args.solve)
		(\e -> print "error")

parse :: String -> IO (Size,[HLine],[VLine])
parse = run parseFile

initMap :: (Size,[HLine],[VLine]) -> IO Map
initMap (s,hs,vs) = do
	lmap'' <- newArray (1,snd s) False :: IO (IOArray Int Bool)
	lmap' <- newArray (1,fst s) lmap'' :: IO (IOArray Int (IOArray Int Bool))
	return $ Map {size = s , hlines = hs, vlines = vs , lmap = lmap'}

mkInitMap :: String -> IO Map
mkInitMap filepath = initMap =<< parse filepath

solve :: Map -> Map
solve = id

showPaint :: FilePath -> Map -> IO()
showPaint file = showAsGUI file
