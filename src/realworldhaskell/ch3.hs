module Ch3 where
import           Data.List

data List a = Cons a (List a)
            | Nil
              deriving (Show)


fromList :: List a -> [a]
fromList (Nil       ) = []
fromList (Cons x Nil) = [x]
fromList (Cons x xs ) = [x] ++ fromList xs

-- data Maybe a = Just a
--              | Nothing

data Tree a = Node a (Maybe a) (Maybe a) deriving (Show)


-- 1
len :: [a] -> Int
len ([]    ) = 0
len ([ x ] ) = 1
len (x : xs) = 1 + len xs

-- 2
-- 3
mean :: [Float] -> Float
mean xs = sum xs / fromIntegral (len xs)
  where
    sum :: [Float] -> Float
    sum []       = 0
    sum [x     ] = x
    sum (x : xs) = x + sum xs

-- 4
ireverse :: [a] -> [a]
ireverse []  = []
ireverse [x] = [x]
ireverse xs  = [last xs] ++ ireverse (init xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ ireverse xs


-- 5
-- TODO
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == ireverse xs

-- 6
sortByLength :: [[a]] -> [[a]]
sortByLength xss = sortBy byLength xss
  where
    byLength :: [a] -> [a] -> Ordering
    byLength xs ys = compare (len xs) (len ys)

-- 7
myintersperse :: a -> [[a]] -> [a]
myintersperse s []           = []
myintersperse s [x]          = x
myintersperse s [x, y]       = x ++ [s] ++ y
myintersperse s (x : y : xs) = x ++ [s] ++ y ++ (myintersperse s xs)
