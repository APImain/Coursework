{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval e (EInt n) = VInt n
eval e (EBool n) = VBool n
eval e (ENil) = VNil
eval e (EVar x) = lookupId x e
eval env (EBin op e1 e2) = (evalOp op v1 v2)
	where 
		v1 = eval env e1
		v2 = eval env e2
eval e (EIf p t f) = helper (eval e p) (eval e t) (eval e f) --this and helper handle if
	where 
	helper :: Value -> Value -> Value -> Value
	helper (VBool p) t f= if p then t else f
	helper _ t f = throw (Error "type error")
eval env (ELet x e1 e2) = eval env' e2
	where
		v = eval env e1
		--evn1 = case v of (Vclos evn2 x e) -> Vclos evn2 x e
           --6131b8e1a18433efc77c2505f668454cebd8e230             --_ -> v
		env' = (x,v) : env
eval env (ELam x body) = VClos env x body --this does closure
eval env (EApp "head" e2) = seperateh (eval env e2)
 where
    seperateh :: Value -> Value
    seperateh (VPair h t) = h
eval env (EApp "tail" e2) = seperatet (eval env e2)
 where
    seperatet :: Value -> Value
    seperatet (VPair h t) = t
	
eval env (EApp e1 e2) = eval (((x, (eval env e2)) :(closEnv ++ env))) body --this just evals 2 things then applies
	where
	(VClos closEnv x body) = eval env e1--(VClos env x e1)--eval to closure
	vArg = eval env e2 --eval arg
	--bodyEnv =    --add x (VClos closEnv x body)
	--first bodyenv implementation  bodyEnv = (add x vArg closEnv)
	
	--add :: Id -> Value -> Env -> Env
	--add x v env = (:) (x,v) env
--
--head' :: Value
--head' =  VPrim(\x->(VInt 1) )
--head _ = error ""
-- take a VPair and choose first entry

--tail' :: Value
--tail' = VPrim(function that gets tale)
--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus (VInt v1) (VInt v2) = VInt((+) v1 v2)
evalOp Minus (VInt v1) (VInt v2) =VInt( (-) v1 v2)
evalOp Mul (VInt v1) (VInt v2) = VInt((*) v1 v2)
evalOp Div (VInt v1) (VInt v2) = VInt ((div) v1 v2)
evalOp Eq (VInt v1) (VInt v2) = if v1 == v2 then (VBool True) else (VBool False)
evalOp Eq (VBool v1) (VBool v2) = if v1 == v2 then (VBool True) else (VBool False)
--evalOp Eq (VPair v1 v2) (VPair a1 a2) = VBool False--not done
evalOp Eq (VPair v1 v2) (VNil) = VBool False
evalOp Eq (VNil) (VNil) = VBool True
evalOp Ne (VInt v1) (VInt v2) = if v1 == v2 then (VBool False) else (VBool True)
evalOp Ne (VBool v1) (VBool v2) = if v1 == v2 then (VBool False) else (VBool True)
evalOp Lt (VInt v1) (VInt v2) = if v1 < v2 then (VBool True) else (VBool False)
evalOp Le (VInt v1) (VInt v2) = if v1 <= v2 then (VBool True) else (VBool False)
evalOp And (VBool v1) (VBool v2) = if v1 && v2 then (VBool True) else (VBool False)
evalOp Or (VBool v1) (VBool v2) = if v1 || v2 then (VBool True) else (VBool False)
--evalOp Cons (VNil) (VNil) =VPair (VNil) (VNil)
evalOp Cons (VInt h) (VNil) = VPair (VInt h) (VNil)
evalOp Cons (VInt h) (VPair x y) = VPair (VInt h) (VPair x y) --support for cons
evalOp _ _ _ = throw (Error "type error")
	--(VInt v1) = eval env e1
	--(VInt v2) = eval env e2
	--(VBool v1) = eval env e1
	--(VBool v2) = eval env e2
	--f = case op of
	--	eq
		--ne
		--Lt
		--Le
		--And
		--Or--do various functions

--------------------------------- -----------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId a [] = throw (Error ("unbound variable: " ++ a))
lookupId  a (x:xs) = if (a==(fst x)) then snd x else lookupId a xs--finds the left most value, actually solve the problem
--normal case
	--lookup :: Id -> Env -> Value
	--literally just return the first found occurence because it is the first one from the left
	--actually find the answer 
--get var from enviroment first part of pair, if var is found return most recent value
--if not return error msg


prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
	-- make a thing in the env called [("head", VPrim(whatever)]
	("head", VPrim(\x ->  x)),--VPrim (\x-> \y-> \z->y VPair (VInt a) (VInt b))) 
	("tail", VPrim(\x ->  x))
	--("tail", tail')
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
