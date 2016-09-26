-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVars :: Expr -> Idents
freeVars
  = visit $
      V {vLit = const S.empty,
         vVar = S.singleton,
         vUnary = const id,
         vBinary = const S.union}

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env
  = visit $
      V {vLit = Lit,
         vVar = \i -> case (lookup i env) of
                        Nothing -> Var i
                        Just(x) -> x,
         vUnary = Unary,
         vBinary = Binary}

eval :: Expr -> Bool
eval
  = visit $
      V {vLit = id,
         vVar = error "free var in expression",
         vUnary = mf1,
         vBinary = mf2}

-- ----------------------------------------
