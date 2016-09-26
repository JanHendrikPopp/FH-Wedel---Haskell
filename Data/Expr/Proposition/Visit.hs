-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visit where

import Data.Expr.Proposition.Types

-- ----------------------------------------

data Visitor r
  = V { vLit    :: Bool  -> r
      , vVar    :: Ident -> r
      , vUnary  :: Op1   -> r -> r
      , vBinary :: Op2   -> r -> r -> r
      }

idExpr :: Visitor Expr
idExpr = V { vLit = Lit,
             vVar = Var,
             vUnary = Unary,
             vBinary = Binary}

visit :: Visitor r -> Expr -> r
visit v (Lit b)           = (vLit v) b
visit v (Var x)           = (vVar v) x
visit v (Unary o e)       = (vUnary v) o (visit v e)
visit v (Binary o e1 e2)  = (vBinary v) o (visit v e1) (visit v e2)

-- ----------------------------------------
