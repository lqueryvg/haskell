
myLen :: [a] -> Int
myLen [] = 0
myLen xs = 1 + myLen (tail xs)

myTot :: [Int] -> Int
myTot [] = 0
myTot (x:xs) = x + (myTot xs)

myMean :: [Int] -> Float
myMean [] = 0
myMean xs = fromIntegral (myTot xs) / fromIntegral (myLen xs)
