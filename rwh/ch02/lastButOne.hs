lastButOne :: [a] -> a
lastButOne [] = error "empty list given"
lastButOne [a] = error "single element list supplied"
lastButOne [a,b] = a
lastButOne (x:xs) = lastButOne xs
