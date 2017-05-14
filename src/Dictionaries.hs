{-# OPTIONS
 
 -XLambdaCase
#-}


module Dictionaries where

import System.IO
import Control.Monad
import Data.List
--import qualified Data.Bimap as B
import Data.CSV
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Text.Read
import Debug.Trace

import IOUtilities
import Utilities
import TypeSyns
import Tables
import ImportHypothesis
import AppCSV

type DLookup = String -> IO (Maybe (M.Map S S))

-- Note: I'm hardcoding in location for now

strToDict :: String -> DLookup
strToDict = \case
            "Chinese" -> chinese
            _ -> chinese
--add more dictionaries later

chinesePath = "C:/Users/oldhe/Dropbox/Learning/languages/chinese/cedict_ts.u8"

stripSpaces :: S -> S
stripSpaces = (>>= (\x -> if x==' ' then [] else [x]))

chinese :: DLookup
chinese s = do
  txt <- readFile chinesePath
  let l = lines txt
  let mline = find (((stripSpaces s)++" ") `isPrefixOf`) l
  case mline of
    Nothing -> return (Nothing |> trace (s++" not found"))
    Just line -> do
               let i = fromMaybe 0 $ elemIndex '[' line
               let j = fromMaybe 0 $ elemIndex ']' line
               let pron = sublist (i+1) j line
               let slashes = elemIndices '/' line --  |> debugShow
               let def = "\""++(sublist ((slashes!!0)+1) (last slashes) line)++"\"" --  |> debugShow
               return $ Just $ M.fromList ([("NonEnglish", s),
                                            ("English", def),
                                            ("Pronunciation", pron),
                                            ("Language", "Chinese")])

updateTableWithEntries :: (M.Map S S) -> [M.Map S S] -> [[S]] -> [[S]]
updateTableWithEntries m entries datas = 
    let 
        identifierKey = ((m M.! "Identifier")) --  |> flip debugSummary ("ik:"++))
        identifierInd = read ((m M.! identifierKey)) --  |> flip debugSummary ("ii:"++))
        noteIDInd = read ((m M.! "NoteID")) --   |> flip debugSummary ("nid:"++)) :: Int
        s = S.fromList $ map (!!identifierInd) datas
        --assume sorted
        lastInd = read ((last datas)!!noteIDInd) --   |> flip debugSummary ("li:"++)) :: I
        --assume each the same length
        len = length (head (datas)) --  |> debugShow))
        reverseM = M.fromList $ mapFilter (\(x,y) -> 
                                    case readMaybe y of 
                                      Just i -> Just (i,x)
                                      Nothing -> Nothing) $ M.toList m
        inds = map (\i -> M.lookup i reverseM) [0..(len - 1)] --  |> debugShow
        entries2 = nubBy (\x y -> x M.! identifierKey == y M.! identifierKey) entries
        newLines = imap (\i entry -> map (\case
                                           Just s -> 
                                               if s=="NoteID"
                                               then show (lastInd+i)
                                               else fromMaybe "" (M.lookup s entry)
                                           Nothing -> "") inds) entries2
    in
      datas++newLines

addVocabToTable :: String -> FilePath -> FilePath -> FilePath -> IO ()
addVocabToTable lang oldVocabFP hypFP converterFP = do
  vocab <- importHyp hypFP
  m <- readConverter converterFP
  entries <- fmap catMaybes $ forM vocab (strToDict lang)
  datas <- parseCSVFile oldVocabFP
  let updatedVocab = updateTableWithEntries m entries datas 
  writeFile oldVocabFP (writeListToCSV (\_ x -> x) updatedVocab)
