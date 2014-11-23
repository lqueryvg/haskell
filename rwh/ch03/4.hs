myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ (myReverse xs)

