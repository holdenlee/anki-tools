module AppCSV where

import System.Environment
import System.IO
import Data.List
import Data.CSV
import Data.Maybe
import Text.ParserCombinators.Parsec

import IOUtilities
import Utilities
import TSV

writeListToSV :: String -> (Int -> a -> [String]) -> [a] -> String
writeListToSV sep f li = unlines $ map (intercalate sep) $ imap f li 

writeListToCSV = writeListToSV ","

writeListToTSV = writeListToSV "\t"

-- fix for the fact that parseFromFile gives an error if there is no newline at the end
parseTSVFile inp = do
  parsed <- parseFromFile tsvFile inp
  case parsed of
    Right p -> return p
    Left _ -> do
                appendFile inp "\n"
                fmap fromRight $ parseFromFile tsvFile inp

parseCSVFile inp = do
  parsed <- parseFromFile csvFile inp
  case parsed of
    Right p -> return p
    Left _ -> do
                appendFile inp "\n"
                fmap fromRight $ parseFromFile csvFile inp

appendToSV :: String -> (Int -> [String] -> [String]) -> String -> String -> IO ()
appendToSV sep f inp out = do
  table <- parseTSVFile inp
  appendFile out (writeListToSV sep f table)

appendToCSV = appendToSV ","

appendToTSV = appendToSV "\t"

{-
main = do
	txt <- readFile "2dnames.txt"
        catalog <- fmap fromRight $ parseFromFile csvFile "2dcatalog.csv"
        let m = M.fromList $ map (\li -> (li!!0, li!!3)) catalog
	let names = map (\x -> (cutFirst ' ' x)++(case M.lookup x m of {Nothing -> []; Just y -> [y]})) (lines txt)
        let sorted = sortBy (\x y -> compare (x!!1) (y!!1)) names
        let unlined = unlines $ map (intercalate ",") sorted
        writeFile "2dnames_out.csv" unlined

-}
