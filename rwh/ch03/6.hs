import Data.List

compareLengths :: [a] -> [a] -> Ordering
compareLengths a b = compare (length a) (length b)

mySort :: [[a]] -> [[a]]
mySort list = Data.List.sortBy compareLengths list

