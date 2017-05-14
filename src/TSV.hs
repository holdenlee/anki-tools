{-# LANGUAGE Safe #-}
{-# OPTIONS
 
 -XRank2Types
#-}

-- modified from https://hackage.haskell.org/package/MissingH-1.4.0.1/docs/src/Data-CSV.html#csvFile

module TSV (tsvFile, genTsvFile) where
import Text.ParserCombinators.Parsec
import Data.List (intersperse)

eol :: forall st. GenParser Char st String
eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> string "\n" <|>
      string "\r" <?> "End of line"

cell :: GenParser Char st String
cell = quotedcell <|> many (noneOf "\t\n\r")

quotedchar :: GenParser Char st Char
quotedchar = noneOf "\""
             <|> (try $ do string "\"\""
                           return '"'
                 )
quotedcell :: CharParser st String
quotedcell = do char '"'
                content <- many quotedchar
                char '"'
                return content

line :: GenParser Char st [String]
line = sepBy cell (char '\t')

tsvFile :: CharParser st [[String]]
tsvFile = endBy line eol

{- | Generate CSV data for a file.  The resulting string can be
written out to disk directly. -}
genTsvFile :: [[String]] -> String
genTsvFile inp =
    unlines . map csvline $ inp
    where csvline :: [String] -> String
          csvline l = concat . intersperse "\t" . map csvcells $ l
          csvcells :: String -> String
          csvcells "" = ""
          csvcells c = '"' : convcell c ++ "\""
          convcell :: String -> String
          convcell c = concatMap convchar c
          convchar '"' = "\"\""
          convchar x = [x]
