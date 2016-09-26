module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env (Lit x)                    = Lit x
substVars env (Unary o e)                = Unary o (substVars env e)
substVars env (Binary o e1 e2)           = Binary o (substVars env e1) (substVars env e2)
substVars [(i,e)] (Var x) | i == x       = e
                          | otherwise    = Var x


freeVars :: Expr -> [Ident]
freeVars (Lit _)          = []
freeVars (Var i)          = [i]
freeVars (Unary _ e)      = freeVars e
freeVars (Binary _ e1 e2) = freeVars e1 ++ freeVars e2

-- ----------------------------------------
