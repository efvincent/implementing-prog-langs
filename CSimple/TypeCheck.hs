module CSimple.TypeCheck where

import CSimple.Lang.ErrM
import CSimple.Lang.Par (myLexer, pProgram)
import System.Exit (exitFailure)
import Data.Map (Map)
import CSimple.Lang.Abs
import CSimple.Lang.Print (printTree)
import Control.Monad (foldM)
import qualified Data.Map as Map

-- Symbol Tables
-- There is no global environment, so the initial empty environment is an empty stack

type Env      = (FnSigs, CtxStack)      -- Function signatures and context stack
type FnSigs   = Map Id FnSig            -- A single function type signature
type FnSig    = ([Type], Type)
type CtxStack = [Ctx]                   -- a stack of contexts
type Ctx      = Map Id Type             -- variables with thier type in one context

lookupVar :: Env -> Id -> Err Type
lookupVar (fnsigs, ctx:rest) ident  =
  case Map.lookup ident ctx of
    Just t -> Ok t
    Nothing -> Bad $ "Variable not declared at " ++ show ident

lookupFun :: Env -> Id -> Err FnSig
lookupFun (fnsigs, ctxStack) ident =
  case Map.lookup ident fnsigs of
    Just fnsig -> Ok fnsig
    Nothing -> Bad $ "Function not found at " ++ show ident

updateVar :: Env -> Id -> Type -> Err Env
updateVar (fnsigs, ctx:rest) ident t = 
  -- if the var exists in the top context in the context stack
  -- that's an error, otherwise add it and we're good
  if Map.member ident ctx
  then Bad $ "Duplicate variable declaration at " ++ show ident
  else 
    let newCtx = Map.insert ident t ctx in
    Ok (fnsigs, newCtx:rest)

-- Add a function sig with an Id to an environemnt
updateFun :: Env -> Id -> FnSig -> Err Env
updateFun (fnsigs, ctxStack) ident fnsig = 
  -- if the function name already exists, that's a failure
  if Map.member ident fnsigs
  then Bad $ "Duplicate function definition at " ++ show ident
  else Ok (Map.insert ident fnsig fnsigs, ctxStack)
  
-- Create a new block (context) in an environment's context stack
newBlock  :: Env -> Env
newBlock (fnSigs, ctxs) = 
  (fnSigs, Map.empty : ctxs)

-- Definition of the empty environment, containing no functions
-- and no contexts
emptyEnv  :: Env
emptyEnv = (Map.empty, []) 

{-
****************************************************
-}

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

checkProg :: Env -> Program  -> Err Env
checkProg = undefined

-- Def = DFun Type Id [Arg] [Stm]
-- Arg = ADecl Type Id
checkFun :: Env -> Def -> Err Env 
checkFun = undefined

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

typecheck :: Program -> Err Env
typecheck (PDefs defs) =
  let env = emptyEnv in
  undefined
  
