import           Data.List


perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:xs' | x <- xs, xs' <- perms (xs\\[x]) ]

qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (h:t) = sort [ x | x <- t, x<=h ] ++ [h] ++ sort [ x | x <- t, x>h ]

repeat :: a -> [a]
repeat a = as
  where
    as = a : as

nats :: [Int]
nats = [0..]

odds :: [Int]
odds = [1,3..]

squares :: [Int]
squares = [ n*n | n <- [0..] ]

factors :: Int -> [Int]
factors n = [ i | i <- [1..n `div` 2], n `mod` i == 0 ]

perfects :: [Int]
perfects = [ n | n <- [1..], sum(factors n) == n ]

primes :: [Int]
primes = sieve [ 2.. ]
 where
   sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p > 0 ]
