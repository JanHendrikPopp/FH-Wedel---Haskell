module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [[x] | x <- ['a'..'z']] ++ [[x] ++ show y | y <- [1..], x <- ['a'..'z']]


-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) xs xy
  where
    xs = tail fibs
    xy = fibs

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = filterPrime [2..]
  where filterPrime (p:xs) = p: filterPrime [ x | x <- xs, x `mod` p /= 0]

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = [x * 2 | x <- [0..]]
      is3 = [x * 3 | x <- [0..]]
      is5 = [x * 5 | x <- [0..]]

merge :: [Integer] -> [Integer] -> [Integer]
merge [] x                      = x
merge x []                      = x
merge (x:xs) (y:ys) | y == x    = merge (x:xs) ys
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)

-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges []     = []
merges (x:xs) = merge x (merges xs)

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = undefined    -- after chapter about folds

-- ----------------------------------------
