module Ch6 where

data Color = Red | Green | Blue
colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

stringEq :: [Char] -> [Char] -> Bool
-- Match if both are empty
stringEq []       []       = True
-- If both start with the same char, check the rest
stringEq (x : xs) (y : ys) = x == y && stringEq xs ys
-- Everything else doesn't match
stringEq _        _        = False

class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool

class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- Minimal complete definition:
    -- (==) or (/=)
    x /= y = not (x == y)
    x == y = not (x /= y)

instance BasicEq3 Color where
    isEqual3 Red   Red   = True
    isEqual3 Green Green = True
    isEqual3 Blue  Blue  = True
    isEqual3 _     _     = False
