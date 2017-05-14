{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Tables where

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

maxCardsInFile :: I
maxCardsInFile = 100000

tableToMap :: [[String]] -> M.Map String String
tableToMap li = M.fromList (map (\l -> (fromMaybe "" (l `mindex` 0) , fromMaybe "" (l `mindex` 1))) li)

tableToOutput :: M.Map S S -> [S] -> [[S]] -> [[S]]
tableToOutput m fields inp = 
    let
        checkF = case (M.lookup "field" m) of
                   Just field -> 
                       let i = read (m M.! field)
                       in (\l -> field `M.member` m && l!!i /= "")
                   Nothing -> const True
        inds = map (\field -> case fmap readMaybe $ M.lookup field m of
                                Nothing -> Left ""
                                Just Nothing -> Left (m M.! field)
                                Just (Just i) -> Right i) fields
        lookupInt str mp = fromMaybe 0 (fmap read $ M.lookup str m)
        globalID = lookupInt "GlobalID" m
        noteIDInd = read (m M.! "NoteID")
    in
      mapMaybe (\l -> 
                    if checkF l
                    then Just $ (show $ maxCardsInFile * globalID + (read (l!!noteIDInd))):(map (\case
                                                                                                  Right i -> fromMaybe "" (l `mindex` i)
                                                                                                  Left s -> s) inds)
                  else Nothing) inp

--field <- maybeToList $ lookup "field" m
readFields :: FilePath -> IO [S]
readFields fp = do
  txt <- S.readFile fp
  return $ lines txt

readConverter :: FilePath -> IO (M.Map S S)
readConverter fp = do
  table <- parseTSVFile fp
  return $ tableToMap table

filterTxtFiles :: S -> [S] -> [(S,S)]
filterTxtFiles dir = mapFilter (\x -> case (splitExtension x) of
                                        (fname, ".txt") -> Just (fname, dir </> fname)
                                        _ -> Nothing)
--map (\x -> (takeBut 4 x, dir++x)) . filter (".txt" `isSuffixOf`)

convertDirectory :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
convertDirectory fieldsFP inDir outDir dataFP = do
    fields <- readFields fieldsFP
    datas <- fmap tail $ parseCSVFile dataFP
    -- li <- listDirectory inDir
    files <- fmap (filterTxtFiles inDir) $ listDirectory inDir
    -- putStrLn (show (inDir, li, files))
    forM_ files (\(name, fullpath) -> do
                   converter <- readConverter (fullpath <.> "txt")
                   let out = tableToOutput converter fields datas
                   let outname = outDir </> name <.> "txt"
                   putStrLn ("writing output to "++outname)
                   writeFile (outname) $ writeListToTSV (\_ x -> id x) out)
