-- | Lab 2: type checker and interpreter for C--.

import System.Environment (getArgs)
import System.Exit        (exitFailure)

import CPP.Abs            (Program)
import CPP.Par            (pProgram, myLexer)
import CPP.ErrM           (Err(Ok,Bad))

import TypeChecker        (typecheck)
import Interpreter        (interpret)

-- | Main: read file passed by only command line argument,
--   parse as C-- code, type check, and interpret.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= parse >>= check >>= interpret
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure

-- | Parse, file contents as C-- program.

parse :: String -> IO Program
parse s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> return tree

-- | Type check C-- abstract syntax tree.

check :: Program -> IO Program
check tree = do
  case typecheck tree of
    Bad err -> do
      putStrLn "TYPE ERROR"
      putStrLn err
      exitFailure
    Ok _ -> return tree
