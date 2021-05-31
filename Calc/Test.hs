-- Program to test parser, automatically generated by BNF Converter.

module Calc.Test (testMain) where

import Prelude
  ( ($)
  , Integer
  , interact
  , Either(..)
  , Int, (>), (+),(-),(*)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, Integral (div)
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import Calc.Abs ( Exp(..) )   
import Calc.Lex   ( Token )
import Calc.Par   ( pExp, myLexer )
import Calc.Print ( Print, printTree )
import Calc.Skel  ()
import Calc.Interpreter
import qualified Calc.ErrM

-- import Calc.ErrM  (Ok)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
      exitSuccess
  where
  ts = myLexer s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

testMain :: IO ()
testMain = do
  interact calc
  putStrLn ""

calc :: String -> String
calc s = 
  let Right e = pExp (myLexer s)
  in show (eval e)


