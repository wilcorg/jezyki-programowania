import Data.List

binominal :: Integer -> Integer -> Integer
binominal n k
  | k == 0 = 1 -- if k == 0
  | k == n = 1 -- if k == n
  | k > n = 0  -- fuzzy case
  | otherwise = binominal (n - 1) (k - 1) + binominal (n - 1) k

---

next_row :: [Integer] -> [Integer]
-- offseted lists sum
-- [0121]
-- [1210]
-- [1331]
next_row row = zipWith (+) ([0] ++ row) (row ++ [0])

-- list of ints with undefined size
pascal_tr :: [[Integer]]
-- iterate : passes default arg [1] to next_row
--           and resuses again as an arg
pascal_tr = iterate next_row [1]

-- Int === int64_t
-- Integer === bigint
binominal2 :: Int -> Int -> Integer
-- operator!! takes nth element from a list
binominal2 n k = (pascal_tr !! n) !! k

---

-- Ord adds lexigoraphical operators to list
merge :: Ord a => [a] -> [a] -> [a]
-- t suffix for "tail" like in prolog
merge [] yt = yt
merge xt [] = xt
merge (x:xt) (y:yt)
    | x <= y    = x : merge xt (y:yt)
    | otherwise = y : merge (x:xt) yt

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xt  = merge (mergesort x) (mergesort y)
  where
    (x, y) = splitAt (length xt `div` 2) xt

---

-- _gcd and _lcm because they're used in Haskell's stdlib
_gcd :: Integer -> Integer -> Integer
_gcd 0 b = b
_gcd a 0 = a
-- `mod` abandoms reverse polish notation
_gcd  a b = _gcd b (a `mod` b)

ext_gcd :: Integer -> Integer -> (Integer, Integer, Integer)
ext_gcd a b
  | b == 0    = (a, 1, 0)
  | otherwise = (g, t, s - q * t)
  where
    (g, s, t) = ext_gcd b (a `mod` b)
    q = a `div` b

_lcm :: Integer -> Integer -> Integer
_lcm a b = abs((a * b) `div` (_gcd a b))

de :: Integer -> Integer -> (Integer, Integer, Integer)
de a b = (x, y, z)
  where
    (g, x, y) = ext_gcd a b
    l = _lcm a b
    z = 1

---

prime_factors :: Integer -> [Integer]
prime_factors n = factor n 2
  where
    factor n p
      | p * p > n      = [n | n > 1]  -- upper search bound
      | n `mod` p == 0 = p : factor (n `div` p) p  -- add p to list
      | otherwise      = factor n (p + 1)

---

totient :: Integer -> Integer
totient n = genericLength [x | x <- [1..n], _gcd x n == 1]  -- returns Integer instead of Int

---

unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs) = x : unique(filter (/= x) xs)

totient2 :: Integer -> Integer
totient2 n = round $ fromIntegral n * product [(1 - 1 / fromIntegral p) | p <- unique (prime_factors n)]

---

primes :: Integer -> [Integer]
primes n = filter (\x -> length (prime_factors x) == 1) [2..n]

