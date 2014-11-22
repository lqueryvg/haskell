-- Find the K'th element of a list
--
elementAt :: (Num k, Ord k) => [a] -> k -> a
elementAt [] _ = error "gone off the end"
elementAt (x:xs) k
    | k <= 0 = error "index too small"
    | k == 1 = x
    | otherwise = elementAt xs (k-1)
