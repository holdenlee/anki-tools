{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Main where

import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
--import qualified Data.Hashable
import Data.Either
import Data.Maybe
import System.Directory
import qualified System.IO.Strict as S
import System.FilePath
import Text.Read

import IOUtilities
import Utilities
import TSV
import AppCSV
import TypeSyns

import Tables


main = do
  args <- getArgs
  let dataFP = if (length args == 0) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/vocab.csv" else (args!!0)
  let fieldsFP = if (length args <= 1) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/fields.txt" else (args!!1)
  let inDir = if (length args <= 2) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/converters" else (args!!2)
  let outDir = if (length args <= 3) then "C:/Users/oldhe/Dropbox/Learning/languages/anki/output" else (args!!3)
  convertDirectory fieldsFP inDir outDir dataFP
