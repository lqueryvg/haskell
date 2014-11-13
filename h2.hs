myButLast :: [a] -> a
myButLast [] = error "no ButLast on empty"
myButLast [x] = error "no ButLast on single element"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs
