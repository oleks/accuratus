module Main where

import Ast
import Parser
import qualified Analyses.SymDiff as SD
import qualified Analyses.Reduce as R

import System.Directory ( doesFileExist )
import System.Environment( getArgs )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint.GenericPretty ( pretty )

report :: String -> IO ()
report = hPutStrLn stderr

exitNoCommand :: IO a
exitNoCommand = do
  report "Tell me what to do!"
  exitWith (ExitFailure 1)

exitInvalidCommand :: String -> IO a
exitInvalidCommand c = do
  report $ c ++ " is not a valid command."
  exitWith (ExitFailure 1)

exitParseError :: ParseError -> IO a
exitParseError e = do
  report $ pretty e
  exitWith (ExitFailure 1)

liftF :: (Prog -> IO ()) -> Either ParseError Prog -> IO ()
liftF f lr =
  case lr of
    Left e -> exitParseError e
    Right p -> f p

with :: String -> (Prog -> IO ()) -> IO ()
with arg f = do
  let f' = liftF f
  isFile <- doesFileExist arg
  if isFile
  then parseFile arg >>= f'
  else f' (parseString arg)

mainParse :: Prog -> IO ()
mainParse = putStrLn . pretty

mainSymDiff :: Prog -> IO ()
mainSymDiff = putStrLn . pretty . SD.allFstDerivs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> exitNoCommand
    ["parse", arg] ->
      with arg $ mainParse
    ["symdiff", arg] ->
      with arg $ mainSymDiff
    (c:_) -> exitInvalidCommand c
