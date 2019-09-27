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
myLength :: [a] -> Int
myLength ([]    ) = 0
myLength ([ x ] ) = 1
myLength (x : xs) = 1 + myLength xs

-- 2
-- 3
mean :: [Float] -> Float
mean xs = sum xs / fromIntegral (myLength xs)
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
-- isPalindrome :: [a] -> Bool
-- isPalindrome xs = xs == ireverse xs
