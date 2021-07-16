-- A generator for immutable JAVA datastructures which simulate Haskell data types
-- Author  : Andreas Abel
-- Created : 2005-10-17
-- Modified: 2016-11-28

module Main where

import Data.Functor  -- for ghc 7.6

import System.Environment
import System.IO

import Syntax
import Lexer
import Parser
import Options
import Printer

main :: IO ()
main = do
  -- Parse command line.
  (opt, src, dest) <- parseCmdLine =<< getArgs
  -- Parse input file.
  ds' <- parser . alexScanTokens <$> readFile src
  -- Optionally add default visitor to each class.
  let ds = if defaultVisitor opt then map addDefaultVisitor ds'
           else ds'
  -- Print the structures to .java files.
  outputClasses opt dest $
    ds >>= \ d -> dataToClasses opt d ++ dataToVisitors opt d

-- | Add default visitor to existing visitors of a data type.
addDefaultVisitor :: Data -> Data
addDefaultVisitor (Data id params cs vs) = Data id params cs $
  Visitor (id ++ "Visitor") (Gen $ fresh params) : vs
  where
  fresh :: [String] -> String
  fresh l = head $ filter (`notElem` l) $ map (:[]) $ ['R' .. 'Z'] ++ ['A' .. 'Q']

-- | Print generated classes to file or to individual files.
outputClasses :: Options -> Maybe String -> [Class] -> IO ()
outputClasses opts dest cs
  | pubClasses opts = mapM_ printToFile cs
  | otherwise       = oneFile dest cs

-- | Each class one file.
printToFile :: Class -> IO ()
printToFile (Class id usesList body) = do
  let name = id ++ ".java"
  createFile name usesList $ body ++ "\n"

-- | One file for all classes.
oneFile :: Maybe String -> [Class] -> IO ()
oneFile mName cs = do
  let contents = cs >>= \ (Class _ _ body) -> body ++ "\n\n"
  maybe (putStr contents)
        (\ name -> createFile name (any classUsesList cs) contents)
        mName

-- | Dump contents into file.
createFile :: String -> Bool -> String -> IO ()
createFile name usesList contents = do
  hPutStrLn stderr ("creating " ++ name)
  writeFile name $ header name usesList ++ contents

-- | Java file header.  Possibly import statement for List.
header :: String -> Bool -> String
header name usesList =
  "// " ++ name ++ "\n// Created by github.com/andreasabel/java-adt\n\n" ++
  if usesList then "import java.util.List;\n\n" else ""
