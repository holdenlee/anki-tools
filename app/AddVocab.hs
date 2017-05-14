module Main where

import System.Environment
import System.IO

--import IOUtilities
--import Utilities
--import TSV
import Dictionaries

main = do  
  args <- getArgs
  let lang = if (length args == 0) then "chinese" else (args!!0)
  let dataFP = if (length args <= 1) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/chinese.csv" else (args!!1)
  let hypFP = if (length args <= 2) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/hypothesis.csv" else (args!!2)
  let fieldsFP = if (length args <= 3) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/chinese_converter.txt" else (args!!3)
  addVocabToTable lang dataFP hypFP fieldsFP
