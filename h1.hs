myLast :: [a] -> a
myLast [] = error "no last on empty"
myLast [x] = x
myLast (_:xs) = myLast xs

