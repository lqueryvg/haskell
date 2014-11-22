-- Find the number of elements of a list
--
myLength :: (Num n) => [a] -> n
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)
