-- Reverse a list
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == myReverse(x)
