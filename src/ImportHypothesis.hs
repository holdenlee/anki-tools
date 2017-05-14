module ImportHypothesis where

import System.IO
import Data.List
--import qualified Data.Bimap as B
import Data.CSV
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

import IOUtilities
import Utilities
import TypeSyns
import Tables
import AppCSV

vocabIndex = 6

importHyp :: FilePath -> IO [String]
importHyp fp = do
  li <- parseCSVFile fp
  let ind = if length (li!!0)==1 then 0 else vocabIndex
  return $ map (!!ind) li
