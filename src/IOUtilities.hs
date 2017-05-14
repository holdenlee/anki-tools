{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

module IOUtilities where
import Data.String.Utils
import System.Environment
import Control.Monad
import System.IO  
import qualified System.IO.Strict as S

operateOnFile :: FilePath -> FilePath -> (String -> String) -> IO ()
operateOnFile a0 a1 f = do
  txt <- S.readFile a0
  writeFile a1 (f txt)

makeCommandWithIOFiles :: String -> (String -> String) -> IO ()
makeCommandWithIOFiles def f = do
  args <- getArgs
  let a0 = if (length args == 0) then def else (args!!0)
  let a1 = if (length args <= 1) then a0 else (args!!1)
  operateOnFile a0 a1 f

mainF :: (String -> String) -> IO ()
mainF f = do
  args <- getArgs
  let inputF = if (length args >= 1) then args !! 0 else "a.in"
  let outputF = if (length args >= 2) then args !! 1 else (take (length inputF - 2) inputF ++ "out")
  operateOnFile inputF outputF f
