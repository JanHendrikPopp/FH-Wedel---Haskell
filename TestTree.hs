module TestTree
where

import           Data.Tree
import           Test.QuickCheck

prop_fromList :: [Int] -> Bool
prop_fromList xs
  = toList (fromList xs) == xs

prop_fromList' :: [Int] -> Bool
prop_fromList' xs
  = toList (fromList' xs) == xs

prop_fromList'' :: [Int] -> Bool
prop_fromList'' xs
  = toList (fromList'' xs) == xs

prop_fromList''' :: [Int] -> Bool
prop_fromList''' xs
  = toList (fromList'' xs) == xs


-- strong balancing criterium
-- length of paths may be differ at most by 1
prop_balance :: Tree Int -> Bool
prop_balance t = minDepth t + 1 >= maxDepth t

-- weaker balancing criterium
-- no path is longer than "ceiling (ld n)"
-- but shorter paths are allowed

prop_balance' :: Tree Int -> Bool
prop_balance' t = d == 0 || s <= c && 2 * s >=c
    where
      d = maxDepth t
      c = 2 ^(d-1)
      s = sizeTree t

-- ----------------------------------------

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheckWith stdArgs{maxSuccess=100}

testFromList :: IO ()
testFromList
  = mapM_ quickCheck'
    [ prop_fromList
    , prop_fromList'
    , prop_fromList''
    , prop_fromList'''
    ]

testBalance :: IO ()
testBalance
  = mapM_ quickCheck'
    [ prop_balance  . fromList   -- strong prop fails with fromList
    , prop_balance  . fromList'
    , prop_balance' . fromList
    , prop_balance' . fromList'
    , prop_balance' . fromList''  -- fails
    , prop_balance' . fromList''' -- fails
    ]

-- ----------------------------------------
