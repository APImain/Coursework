{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.Egg.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import           Text.Printf        (printf)
import           Language.Egg.Types
import           Language.Egg.Utils

--------------------------------------------------------------------------------
check :: BareProgram -> BareProgram
--------------------------------------------------------------------------------
check p = case wellFormed p of
            [] -> p
            es -> throw es

-- | Map from function name to arity
type FunEnv = Env

--------------------------------------------------------------------------------
-- | `wellFormed p` returns the list of errors for a program `p`
--------------------------------------------------------------------------------
wellFormed :: BareProgram -> [UserError]
--------------------------------------------------------------------------------
wellFormed (Prog ds e) = duplicateFunErrors ds
                      ++ concatMap (wellFormedD fEnv) ds
                      ++ wellFormedE fEnv emptyEnv e
  where
    fEnv               = fromListEnv [(bindId f, length xs) | Decl f xs _ _ <- ds]

--------------------------------------------------------------------------------
-- | `wellFormedD fEnv vEnv d` returns the list of errors for a func-decl `d`
--------------------------------------------------------------------------------
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) =  (checkParam xs emptyEnv) ++ wellFormedE fEnv vEnv e -- 4 params first is bind second is list of binds third is function body and 4th is a label
  where    
   vEnv                         = helper xs emptyEnv 
    where
     helper :: [Bind a] -> Env -> Env -- this is fine now
     helper [] env = env
     helper (y:ys) env = helper ys (addEnv y env)
   checkParam ::[Bind a]->Env -> [UserError] -- checks if dup params in env theoretically
   checkParam [] env = []
   checkParam (z:zs) env = if (memberEnv (bindId z) env) then [] ++ checkParam zs env --error with errDupParam z need to re add later dupparam is fucked I dont know why
   else checkParam zs (addEnv z env)
    where 
     (Bind a l) = z

--------------------------------------------------------------------------------
-- | `wellFormedE vEnv e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv env e = go env e
  where    
   gos vEnv es               = concatMap (go vEnv) es    
   go _    (Boolean {})      = []    
   go _    (Number  n     l) = if (n > maxInt) then [errLargeNum l n] else [] --add check here on if number too large    
   go vEnv (Id      x     l) = if (memberEnv x vEnv) then [] else [errUnboundVar l x]   --lookup if x is in venv if not, unboundvar
   -- second check to see if this bind has aleady occured
   go vEnv (Tuple es l) = gos vEnv es
   --check all of the items in tuple for issues 
   go vEnv (GetItem e1 e2 l) = gos vEnv [e1, e2]
   --will add look up feature later sort differnces first
   go vEnv (Prim1 _ e     _) = go  vEnv e    
   go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]    
   go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]    
   go vEnv (Let x e1 e2   _) = if (memberEnv (bindId x) vEnv) then [errDupBind x] ++go vEnv e1 ++ go (addEnv x vEnv) e2 else go vEnv e1 ++ go (addEnv x vEnv) e2  --this is where dup bind works
   go vEnv (App f es      l) =   if (memberEnv f fEnv) then (arityErr) ++ gos vEnv es else (arityErr) ++ gos vEnv es --   do callarity here
    where 
     --arityErr = if ((length es)== argC ) then [] else [errCallArity l f]
     arityErr = 
      case lookupEnv f fEnv of 
       Just n -> if ((length es)== n ) then [] else [errCallArity l f]
       Nothing -> [errUnboundFun l f]

--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors
  = fmap errDupFun
  . concat
  . dupBy (bindId . fName)

-- | `maxInt` is the largest number you can represent with 31 bits (accounting for sign
--    and the tag bit.

maxInt :: Integer
maxInt = 1073741823

--------------------------------------------------------------------------------
-- | Error Constructors: Use these functions to construct `UserError` values
--   when the corresponding situation arises. e.g see how `errDupFun` is used.
--------------------------------------------------------------------------------

errDupFun d       = mkError (printf "duplicate function '%s'" (pprint f))    (sourceSpan f) where f = fName d
errDupParam     x = mkError (printf "duplicate parameter '%s'" (bindId x)) (sourceSpan x)
errDupBind      x = mkError (printf "shadow binding '%s'" (bindId x))      (sourceSpan x)
errLargeNum   l n = mkError (printf "number '%d' is too large" n) l
errUnboundVar l x = mkError (printf "unbound variable '%s'" x) l
errUnboundFun l f = mkError (printf "function '%s' is not defined" f) l
errCallArity  l f = mkError (printf "wrong arity of arguments at call of %s" f) l
