{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | data type invariant

invTree :: Tree a -> Bool
invTree Null = True
invTree (Tip x) = True
invTree (Bin Null _) = False
invTree (Bin _ Null) = False
invTree (Bin l r) = invTree l && invTree r

-- | smart constructor
bin :: Tree a -> Tree a -> Tree a
bin  l Null     = l
bin Null r      = r
bin l r         = Bin l r

instance Functor Tree where
  fmap f Null      = Null
  fmap f (Tip x)   = Tip (f x)
  fmap f (Bin l r) = Bin (fmap f l) (fmap f r)

instance Applicative Tree where
  pure  = Tip
  --Null <*>         = Null

  (<*>) = undefined

instance Monad Tree where
  return x          = Tip x
  Null >>= _        = Null
  Tip x >>= f       = f x
  (Bin l r) >>= f   = Bin (l >>= f) (r >>= f)
  --_    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = Null
  mplus = bin

instance Monoid (Tree a) where
  mempty  = Null
  mappend = bin

-- fold elements like in a list from right to left
instance Foldable Tree where
  foldr _ e t = undefined
  foldr op e Null       = e
  foldr op e (Tip x)    = op x e
  foldr op e (Bin l r)  = foldr op (foldr op e r) l

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null       = e
    visit' (Tip x)    = tf x
    visit' (Bin l r)  = bf (visitTree e tf bf l) (visitTree e tf bf r)

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (const 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (const 1) (\ l r -> 1 + min l r)
maxDepth = visitTree 0 (const 1) (\ l r -> 1 + max l r)

-- ----------------------------------------
-- access functions

viewL :: Tree a -> Maybe (a, Tree a)
viewL Null      = Nothing
viewL (Tip a)   = Just (a, Null)
viewL (Bin l r) = Just (x, bin t r)
        where Just(x, t) = viewL l

viewR :: Tree a -> Maybe (Tree a, a)
viewR Null      = Nothing
viewR (Tip a)   = Just (Null, a)
viewR (Bin l r) = Just (bin t l, x)
        where Just(t, x) = viewR r

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr undefined undefined

-- | runs in O(n^2) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree [] (\x -> [x]) (++)

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- weak balancing criterion
fromList :: [a] -> Tree a
fromList xs     = toTree (toList (map (\x -> Tip x) xs))
  where toList (x1:x2:x3:[])  = [bin x1 x2] ++ [x3]
        toList (x1:x2:[])     = [bin x1 x2]
        toList (x1:x2:xs)     = toList ([bin x1 x2] ++ toList xs)
        toList [x]            = [x]
        toTree (x1:x2:[])     = bin x1 x2
        toTree [x]            = x

fromList1 :: [a] -> [Tree a]
fromList1 xs     = toList (map (\x -> Tip x) xs)
  where toList (x1:x2:x3:[])  = [bin x1 x2] ++ [x3]
        toList (x1:x2:[])     = [bin x1 x2]
        toList (x1:x2:xs)     = toList ([bin x1 x2] ++ toList xs)
        toList [x]            = [x]

  --where toTree (x1:x2:x3:[]) = [(bin x1 x2)] ++ [x3]
  --      toTree (x1:x2:[])    = bin x1 x2
  --      toTree (x1:x2:xs)    = toTree ([(bin x1 x2)] ++ [toTree xs])
  --      toTree [x]           = bin x Null

--fromList []                        = Null
--fromList [x]                       = Tip x
--fromList (x1:x2:[])                = bin (Tip x1) (Tip x2)
--fromList (x1:x2:xs)                =

-- strong balancing criterion
fromList' :: [a] -> Tree a
fromList' []      = Null
fromList' [x]     = Tip x
fromList' xs      = bin (fromList'(fst (split xs))) (fromList'(snd (split xs)))
      where split l = splitAt (((length l) + 1) `div` 2) l

-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
