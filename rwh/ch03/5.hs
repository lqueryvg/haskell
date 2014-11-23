myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome (xs)
    | xs == (myReverse xs)  =  True
    | otherwise                =  False
