module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
            | x1 > x2   = overlap (x2, y2) (x1, y1)
            | otherwise = y1 + 1 >= x2


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2) = y1 < x2


nullInterval :: Interval -> Bool
nullInterval (x, y) = x > y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (min x1 x2, max y1 y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv []        = True
inv (i1 : is) = not (nullInterval i1)
                && (null is || ( not (overlap i1 (head is))
                                 && less i1 (head is)
                                 && inv is))

-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval i []       = [i]
insertInterval i (x:xs)
          | overlap i x   = insertInterval (merge i x) xs
          | less i x      = i : x : xs
          | otherwise     = x : insertInterval i xs


fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList []       = []
fromIntervalList (x:xs)   = insertInterval x (fromIntervalList xs)


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union a []      = a
union [] a      = a
union (x:xs) b  = union xs (insertInterval x b)


member :: Int -> IntervalSet -> Bool
member i []                               = False
member i (x:xs) | overlap x (i,i) = True
                | otherwise               = member i xs


fromList :: [Int] -> IntervalSet
fromList []       = []
fromList (x:xs)   = insert x (fromList xs)


toList :: IntervalSet -> [Int]
toList []     = []
toList (x:xs) = generateList x ++ toList xs
  where generateList (f,t) = [f..t]


-- ----------------------------------------
