module CSimple.TypeCheck where

import CSimple.Lang.ErrM
import CSimple.Lang.Par (myLexer, pProgram)
import System.Exit (exitFailure)
import Data.Map (Map)
import CSimple.Lang.Abs
import CSimple.Lang.Print (printTree)
import Control.Monad (foldM)

-- Symbol Tables
type Env = (Sig,[Context])          -- Stack of functions and context
type Sig = Map Id ([Type], Type)    -- function type signature
type Context = Map Id Type          -- variables with thier type

lookupVar :: Env -> Id -> Err Type
lookupFun :: Env -> Id -> Err ([Type], Type)
updateVar :: Env -> Id -> Type -> Err Env
updateFun :: Env -> Id -> ([Type], Type) -> Err Env
newBlock  :: Env -> Env
emptyEnv  :: Env

lookupVar = undefined
lookupFun = undefined
updateVar = undefined
updateFun = undefined
newBlock  = undefined
emptyEnv  = undefined

inferExp :: Env -> Exp -> Err Type
inferExp env x =
  case x of
    ETrue -> return Tbool
    EInt n -> return Tint
    EId id -> lookupVar env id
    EAdd exp1 exp2 ->
      inferBin [Tint, Tdouble, Tstring] env exp1 exp2

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = 
  do
    typ <- inferExp env exp1
    if typ `elem` types
      then checkExp env exp2 typ
      else fail $ "wrong type of expression " ++ printTree exp1
    return typ

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env exp typ =
  do
    typ2 <- inferExp env exp
    if typ2 == typ
      then return ()
      else fail $ "type of " ++ printTree exp ++
                  " expected " ++ printTree typ ++
                  " but found " ++ printTree typ2

checkStm :: Env -> Stm -> Err Env
checkStm env s = case s of
  SExp exp -> do
    inferExp env exp
    return env
  SDecls typ idents ->
    -- SDecls is a list of variable defs, ex: `int x,y,z;`
    -- they all have the same type, so we can monadic fold over
    -- the identifiers `x,y,z` with the updateVar function which 
    -- will continue so long as not Err, accumulating the environment,
    -- setting each variable to the type. It's awesome haskell monad
    foldM (\env ident -> updateVar env ident typ) env idents
  SWhile exp stm -> do
    checkExp env exp Tbool
    checkStm (newBlock env) stm
    return env
    
compile :: String -> IO ()
compile s = 
  case pProgram (myLexer s) of
    Bad err -> do
      putStrLn "Syntax Error"
      putStrLn err
      exitFailure
    Ok tree ->
      case typecheck tree of
        Bad err -> do
          putStrLn "Type Error"
          putStrLn err
          exitFailure
        Ok _ ->
          putStrLn "OK"   -- or goto next compiler phase

typecheck :: Program -> Err ()
typecheck = error "not implemented"
