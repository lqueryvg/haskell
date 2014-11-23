myLen [] = 0
myLen xs = 1 + myLen (tail xs)
