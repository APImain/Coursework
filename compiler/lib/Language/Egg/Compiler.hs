{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Egg.Compiler ( compiler, compile ) where

import           Data.Monoid
import           Control.Arrow                    ((>>>))
import           Prelude                  hiding (compare)
import           Control.Monad                   (void)
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Egg.Types
import           Language.Egg.Parser     (parse)
import           Language.Egg.Checker    (check, errUnboundVar)
import           Language.Egg.Normalizer (anormal)
import           Language.Egg.Label
import           Language.Egg.Asm        (asm)


--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> check >>> anormal >>> tag >>> tails >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Ann@
--------------------------------------------------------------------------------
type Ann   = ((SourceSpan, Int), Bool)
type AExp  = AnfExpr Ann
type IExp  = ImmExpr Ann
type ABind = Bind    Ann
type ADcl  = Decl    Ann
type APgm  = Program Ann

instance Located Ann where
  sourceSpan = fst . fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

annTag :: Ann -> Int
annTag = snd . fst

annTail :: Ann -> Bool
annTail = snd


--------------------------------------------------------------------------------
compile :: APgm -> [Instruction]
--------------------------------------------------------------------------------
compile (Prog ds e) = compileBody emptyEnv e ++ concatMap compileDecl ds

compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e l) = ILabel (DefStart (bindId f) (0)): compileBody (paramsEnv xs) e 

compileBody :: Env -> AExp -> [Instruction]
compileBody env e = funInstrs (countVars e) (compileEnv env e)

paramsEnv :: [Bind a] -> Env
paramsEnv xs = fromListEnv (zip xids [-2, -3..])
 where
  xids = map bindId xs
-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.

funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs
  = funEntry n
 ++ instrs
 ++ funExit
 ++ [IRet]

-- FILL: insert instructions for setting up stack for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n = [IPush (Reg EBP), IMov (Reg EBP) (Reg ESP), ISub (Reg ESP) (Const (4 * n))]

-- FILL: clean up stack & labels for jumping to error
funExit :: [Instruction]
funExit = [IMov (Reg ESP) (Reg EBP), IPop (Reg EBP)]

--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
--compile notes/things that need to be added and or changed
--funcs to modify:equals, print
--funcs to add: getitem, istuple
--note get item deals with a tuple as first arg and desired index as second one
--note the isX functions need modification as the tag bits have slightly changed
--check on if let and function call (app)
--getitem and tuple creation is done finally
--isbool and istuple should be fine now
compileEnv env v@Number {}       = [ compileImm env v  ]

compileEnv env v@Boolean {}      = [ compileImm env v  ]

compileEnv env v@Id {}           = [ compileImm env v  ]

compileEnv env e@Let {}          = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

--first portion of doing tuples tuple alloc tuplecopy and set tag implemented later
compileEnv env (Tuple es l) = (tupleAlloc (length es))++ tupleCopy 0 (Const (length es)) ++ helper 1 es  ++ setTag (Reg EAX) TTuple
--tuplealloc might need an int so it knows how big of a thing we dealing with
 where
  helper :: Int -> [IExp] -> [Instruction]
  helper c [] = []
  helper c (v:vs) = tupleCopy c (immArg env v) ++ helper (c + 1) (vs)
-- need to make sure this is staying 8 byte aligned so if even number of things in tuple add some white space
--even number * 4 byptes +first 4 bytes for count mean shit isnt aligned
--beginning of getitem currently for pairs only
compileEnv env (GetItem e1 e2 l) =  assertType env e1 TTuple-- 2. load pointer into eax
 ++ [ IMov (Reg EAX) (immArg env e1) ] --3. remove tag bit toT get address
 ++ unsetTag (Reg EAX) TTuple ++ [ IMov (Reg EAX) (tupleAddr (div (c+2) 2 )) ]   -- 4. copy value from resp. slot to eax
  where --remember int is stored with least signifcant bit as zero
   (Const c) = (immArg env e2) -- get arg and do calcs 
--some issue when trying to get the even indexes index
--type issue e2 isnt int so what is it? is it always a const int?
--temp place holder until it is fixed

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v --prim1 does not need to be tail call

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2 -- prim2 doesnt need tail call


compileEnv env (If v e1 e2 l)    = compileIf l env v e1 e2
--deffun replaced by defstart and defend
compileEnv env (App f vs l) = call (DefStart f (0)) [param env v | v <- vs] -- | (snd(l) == True) = [IPush (Reg EBP), IMov (Reg EBP) (Reg ESP)] ++ tailCall (DefFun f) [param env v | v <- vs] 
-- (helper (reverse [param env v | v <- vs]) )
 -- where
   --helper :: [Arg] -> [Instruction]
   --helper [] = []
   --helper (y:ys) = [IPush (Sized DWordPtr y)] ++ helper ys 
-- a couple of the things cant be tail call on e1 and e2 of if e2 on let and nothing else

--compilePrim1, compilePrim2, compileIf, call, tail call
--other functions
--prim1 implementation
compilePrim1 :: annTag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1 v =(assertType env v TNumber) ++ (compileEnv env v ) ++ [ IAdd (Reg EAX) (Const 2), IJo (DynamicErr ArithOverflow) ] 
compilePrim1 l env Sub1 v =(assertType env v TNumber) ++ (compileEnv env v ) ++ [ IAdd (Reg EAX) (Const (-2)), IJo (DynamicErr ArithOverflow) ]
compilePrim1 l env Print v = [IMov (Reg EAX) (immArg env v) ] ++ [IPush (Reg EAX)]  -- there is something wrong with this thing right now 
 ++ [ICall (Builtin "print")] ++ [IAdd (Reg ESP) (Const(4))] --should be push arg call target then add esp 4*n for number of args
compilePrim1 l env IsNum v = [IMov (Reg EAX) (immArg env v)    
 , IMov (Reg EBX) (Reg EAX)    
 , IAnd (Reg EBX) (HexConst 0x00000001)    
 , ICmp (Reg EBX) (Const 1)
 , IJne (Builtin "num_t")
 , IMov (Reg EAX ) (HexConst 0x7fffffff)
 , IJmp (Builtin "num_d")
 , ILabel (Builtin "num_t") 
 , IMov (Reg EAX ) (HexConst 0xffffffff)
 , ILabel (Builtin "num_d") 
 ]
--might be an isue with implementation on this is getting some weirdness in test cases
--resolved isbool now works only on bools not on tuples
compilePrim1 l env IsBool v = [IMov (Reg EAX) (immArg env v)    
 , IMov (Reg EBX) (Reg EAX)    
 , IAnd (Reg EBX) (HexConst 0x00000007)    
 , ICmp (Reg EBX) (Const 7)
 , IJe (Builtin "bool_t")
 , IMov (Reg EAX ) (HexConst 0x7fffffff)
 , IJmp (Builtin "bool_d")
 , ILabel (Builtin "bool_t") 
 , IMov (Reg EAX ) (HexConst 0xffffffff)
 , ILabel (Builtin "bool_d")  
 ]
--is tuple now good to go sdf
compilePrim1 l env IsTuple v = [IMov (Reg EAX) (immArg env v)    
 , IMov (Reg EBX) (Reg EAX)    
 , IAnd (Reg EBX) (HexConst 0x00000007)    
 , ICmp (Reg EBX) (Const 1)
 , IJe (Builtin "tuple_t")
 , IMov (Reg EAX ) (HexConst 0x7fffffff)
 , IJmp (Builtin "tuple_d")
 , ILabel (Builtin "tuple_t") 
 , IMov (Reg EAX ) (HexConst 0xffffffff)
 , ILabel (Builtin "tuple_d")  
 ]
--prim2 implementation
compilePrim2 :: annTag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus  v1 v2 =(assertType env v1 TNumber) ++ (assertType env v2 TNumber) ++ [ IMov (Reg EAX) (immArg env v1), (IAdd) (Reg EAX) (immArg env v2), IJo (DynamicErr ArithOverflow)]
compilePrim2 l env Minus v1 v2 =(assertType env v1 TNumber) ++ (assertType env v2 TNumber) ++ [ IMov (Reg EAX) (immArg env v1), (ISub) (Reg EAX) (immArg env v2), IJo (DynamicErr ArithOverflow)]
--there needs to be a thing to fix excess mults
compilePrim2 l env Times v1 v2 =(assertType env v1 TNumber) ++ (assertType env v2 TNumber) ++ [ IMov (Reg EAX) (immArg env v1), (IMul) (Reg EAX) (immArg env v2), IJo (DynamicErr ArithOverflow)]
 ++ [ISar (Reg EAX) (Const 1)]
--new comparrisons
compilePrim2 l env Equal v1 v2 =  [IMov (Reg EAX) (immArg env v1), ICmp (Reg EAX) (immArg env v2)
  , IJne (Builtin "eq_t")
 , IMov (Reg EAX ) (HexConst 0xffffffff)
 , IJmp (Builtin "eq_d")
 , ILabel (Builtin "eq_t") 
 , IMov (Reg EAX ) (HexConst 0x7fffffff)
 , ILabel (Builtin "eq_d") 
 ]
--removed error checking that only allowed ints now allows all types
compilePrim2 l env Less v1 v2 =(assertType env v1 TNumber) ++ (assertType env v2 TNumber) ++  [IMov (Reg EAX) (immArg env v1), ISub (Reg EAX) (immArg env v2)]
 ++ [IAnd (Reg EAX) (HexConst 0x80000000), IOr (Reg EAX) (HexConst 0x00000001), IOr (Reg EAX) (HexConst 0x7ffffffe)]
compilePrim2 l env Greater v1 v2 =(assertType env v1 TNumber) ++ (assertType env v2 TNumber) ++  [IMov (Reg EAX) (immArg env v2), ISub (Reg EAX) (immArg env v1)]
 ++ [IAnd (Reg EAX) (HexConst 0x80000000), IOr (Reg EAX) (HexConst 0x00000001), IOr (Reg EAX) (HexConst 0x7ffffffe)]
--add stuff for if when ready
compileIf :: Ann -> Env -> IExp -> AExp -> AExp -> [Instruction] --issue with compile if
compileIf l env v e1 e2 = (assertType env v TBoolean) ++ [IMov (Reg EAX) (immArg env v)]
 ++ [ICmp (Reg EAX) (HexConst 0x7fffffff), IJne (BranchTrue (annTag l))]
 ++ compileEnv env e2 ++ [ IJmp (BranchDone (annTag l)) ] 
 ++ ILabel (BranchTrue (annTag l)) 
 : compileEnv env e1 ++ 
 [ ILabel (BranchDone (annTag l)) ] 
--call stuff
call :: Label -> [Arg] -> [Instruction] 
call l (xs) = (helper (reverse xs))++ [ICall (l), IAdd (Reg ESP) (Const ((length xs)*4))]
 where
  helper :: [Arg] -> [Instruction]
  helper [] = []
  helper (y:ys) = [IPush (Sized DWordPtr y)] ++ helper ys
--tail call function called on last 3 tests
tailCall :: Label -> [Arg]-> [Instruction]
tailCall l (xs) = pushS (reverse xs) ++ helper 0 (xs) ++ funExit ++ [IJmp (l)] --pushS (reverse xs) ++ 
 where
  helper :: Int -> [Arg] -> [Instruction]
  helper i [] = []
  helper i (y:ys) = [IMov (Reg EAX) (RegOffset (-4*((length xs) - i)) EBP), --ISub (Reg EAX) (Const (4*((length xs) - i))), --length xs) - 
    IMov (RegOffset (4* ((length xs) - i + 1)) EBP) (Reg EAX)] --IAdd (Reg EBP) (Const (4* ((length xs) - i + 1))),
   ++ helper (i+1) (ys)
  pushS :: [Arg] -> [Instruction]
  pushS [] = []
  pushS (y:ys) = [IPush (Sized DWordPtr y)] ++ pushS ys
   -- push before hand in from arg n at lowest stack address down to arg 1 at address closest to ESP
   -- then we want to do a move opperation where we switch the stuff from the stack with the parameters
   -- this happens through the call helper recursively starting at last arg and working forward
   -- after we do the swapping we reset using funExit and then Jmp instead of a call

--code to implement stuff for tuples
tupleAlloc ::Int -> [Instruction]
tupleAlloc  c  = [ IMov (Reg EAX) (Reg ESI), IAdd (Reg ESI) (Const (4*(c+1))) ] ++ offset
 where 
  offset = if ((isEven) == 0) then [IAdd (Reg ESI) (Const 4)] else [] -- add an extra 4 bytes to do 8 byte alignment
  isEven = mod c 2
-- offset should now be correct
-- copy current "free address" `esi` into `eax`
--add n*4 bytes for n words in tuple plus one for number of elements
tupleCopy ::Int->Arg->[Instruction]
tupleCopy fld a= [ IMov (Reg EBX)    a    , IMov (tupleAddr fld) (Reg EBX)    ]

tupleAddr ::Int->Arg
tupleAddr fld =Sized DWordPtr (RegOffset (4* fld) EAX)

unsetTag ::arg->Ty->[Instruction]
unsetTag r ty =[ISub (Reg EAX) (typeTag ty)]
--fieldOffset :: Field -> Int
--fieldOffset First = 0 
--fieldOffset Second = 1
 
setTag ::Arg->Ty->[Instruction]
setTag r ty = [ IAdd (r) (typeTag ty) ]

--error managment
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty= [ IMov (Reg EAX) (immArg env v)    
 , IMov (Reg EBX) (Reg EAX)    
 , IAnd (Reg EBX) (typeMask ty)    
 , ICmp (Reg EBX) (typeTag  ty)    
 , IJne (DynamicErr (TypeError ty))   
 ] -- dynerr
--

compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is <> is') bs
  where
    (env', is')            = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      <> [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env

immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " <> show (void e)

param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)

stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP


--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Bool where
  repr True  = HexConst 0xffffffff
  repr False = HexConst 0x7fffffff

instance Repr Int where
  repr n = Const (fromIntegral (shift n 1))

instance Repr Integer where
  repr n = Const (fromIntegral (shift n 1))

typeTag :: Ty -> Arg
typeTag TNumber   = HexConst 0x00000000
typeTag TBoolean  = HexConst 0x7fffffff
typeTag TTuple    = HexConst 0x00000001

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
typeMask TTuple   = HexConst 0x00000007
