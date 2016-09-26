-- ----------------------------------------
--
-- simple operations on lists

module Data.List.SimpleOps
where

import Prelude hiding (splitAt)

-- ----------------------------------------

-- | The nub function removes duplicate elements from a list.
--
-- In particular, it keeps only the first occurrence of each element.
-- (The name nub means `essence'.)
--
-- Complexity class?

-- .1 nub with filter

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub ( filter (/= x) xs)


-- .2 nub with list comprehension

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x : nub' [ y | y <- xs, x /= y]


-- .3 nub with foldr
-- after chapter about folds

nub'' :: Eq a => [a] -> [a]
nub'' = foldr (\x xs -> x : [ y | y <- xs, x /= y]) []


-- ----------------------------------------

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.

-- the spec
splitAt :: Int -> [a] -> ([a],[a])
splitAt i xs = (take i xs, drop i xs)

-- the impl
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' i [] = ([], [])
splitAt' i (x:xs)
      |  i == -1  = ([], (x:xs))
      |  i == 0   = ([], (x:xs))
      | otherwise = ([x] ++ (fst (res xs)), snd (res xs))
        where res xs = splitAt' (i - 1) xs

-- ----------------------------------------

-- | 'intercalate' inserts the list @xs@ in between
-- the lists in @xss@ and concatenates the
-- result.

-- 1. impl: direct or with map
intercalate :: [a] -> [[a]] -> [a]
intercalate _ (x:[]) = x
intercalate a (x:xs) = concat [x, a, intercalate a xs]
intercalate _ []     = []


-- 2. impl: with foldr
-- after chapter about folds
intercalate' :: [a] -> [[a]] -> [a]
intercalate' x = foldr inter []
  where inter y []  = y
        inter y ys  = y ++ x ++ ys
-- ----------------------------------------

-- | The 'partition' function takes a predicate and a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--

-- the spec
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs
  = (filter p xs, filter (not . p) xs)

-- 1. impl: direct
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = ([ x | x <- xs, p x] , [ x | x <- xs, (not . p) x])

-- 2. impl: with foldr
-- after chapter about folds

partition'' :: (a -> Bool) -> [a] -> ([a], [a])
partition'' p = foldr part ([], [])
  where part x (xs, ys)
            | p x       = (x:xs, ys)
            | otherwise = (xs, x:ys)


-- ----------------------------------------
--
-- | all prefixes of a list

-- 1. impl: direct

inits        :: [a] -> [[a]]
inits []      = [[]]
inits (x:xs)  = []: map (x:) (inits xs)

-- 2. impl: with foldr
-- after chapter about folds
inits'        :: [a] -> [[a]]
inits' = foldr ini []
  where
    ini x [] = [[], [x]]
    ini x xs = []: map (x:) xs

-- ----------------------------------------

-- | concatenates 2 lists of strings
-- with a given char in between the elements
--
-- the following law must hold for split and join
--
--   join' c (split' c xs) == xs
--
--   join' c . split c == id
--

join' :: a -> [[a]] -> [a]
join' _ []      = []
join' _ (x:[])  = x
join' c (x:xs)  = x ++ [c] ++ (join' c xs)

-- | splits the input into sublists at delimiter
--   1. arg is the delimiter
--   the delimiter does not occur in elements of result list

split' :: Eq a => a -> [a] -> [[a]]
split' _ []     = []
split' _ (x:[]) = [[x]]
split' c (x:xs) | c == x    = split' c xs
                | otherwise = [x] : (split' c xs)

-- ----------------------------------------
