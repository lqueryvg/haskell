
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- takes any value of type [a], and produces a value of type List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- takes a value of type List a, and generates a [a]
toList :: List a -> [a]
toList (Cons a b) = a : (toList b)
toList Nil = []
