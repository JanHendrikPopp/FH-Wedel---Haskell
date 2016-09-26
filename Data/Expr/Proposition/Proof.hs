module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable n
      | n == 0      = [[]]
      | otherwise   = map (True:) tt ++ map (False:) tt
        where tt = truthTable (n - 1)

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e = pr e (tt e)

  where
    tt e                 = map (zip (freeVars e)) (map (map Lit) $ truthTable $ length (freeVars e))
    subst e (x:xs)       = subst (substVars [x] e) xs
    subst e [x]          = substVars [x] e
    subst e []           = e
    pr e [[]]            = case eval e of
                            True  -> Nothing
                            False -> Just ([])
    pr e []              = Nothing
    pr e (x:xs)          = case eval (subst e x) of
                              True  -> pr e xs
                              False -> Just (x)

proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------
