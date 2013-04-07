module FileParser (run,parseFile) where

import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Text.Parsec.String
import qualified Interface as I
import Data.Array.IO

type MapSeed = (I.Size,[I.VLine],[I.HLine])

run :: Parser MapSeed -> String -> IO MapSeed
run p contents = case parse p "" contents of
	Left err -> do
		putStrLn "file is broken. Detail is..." 
		print err
		return undefined
	Right x -> return x

parseFile :: Parser MapSeed
parseFile = do
	--parse file
	sz@(w,h) <- size
	newline
	vLines <- readLines w  --read vertical lines
	hLines <- readLines h -- read horizontal lines

	--return map and keys of each line
	return (sz,vLines,hLines)

size :: Parser I.Size
size = (,) <$> (char '(' *> num) <*> (char ',' *> num <* char ')')

readLines :: Int -> Parser [I.Line]
readLines 0 = return []
readLines w = (:) <$> readLine <*> readLines (w-1)

readLine :: Parser I.Line
readLine = do
	v <- (newline >> return Nothing) <|> (Just <$> (spaces *> num))
	case v of
		Just n -> (n:) <$> readLine
		Nothing -> return []

num :: Parser Int
num = read <$> many1 digit
