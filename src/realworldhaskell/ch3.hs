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
