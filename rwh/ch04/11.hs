import Data.Char (digitToInt, isDigit)
import Data.Either (rights)

loop :: Int -> String -> Int

asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = loop acc' xs
    where acc' = acc * 10 + digitToInt x
    
asInt_fold :: String -> Int
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl f 0 xs
    where f tot c = (tot * 10) + (digitToInt c)

type ErrorMessage = String


-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b

myFolder :: Char -> Either ErrorMessage Int -> Either ErrorMessage Int
myFolder c (Left message) = Left message
myFolder c (Right tot)
    | isDigit c   = Right $ (tot * 10) + (digitToInt c)
    | otherwise   = Left $ "non-digit '" ++ [c] ++ "'"

asInt_either :: String -> Either ErrorMessage Int
asInt_either ""       = Left "empty string"
asInt_either ('-':xs) =
    case (asInt_either $ xs) of
        Left message -> Left message
        Right answer -> Right ((-1) * answer)
asInt_either xs = foldr myFolder (Right 0) $ reverse xs


myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat xxs = foldr f [] xxs
    where
        f x acc = acc ++ x


myTakeWhile           :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []      = []
myTakeWhile f (x:xs)
    | f x             = x : (myTakeWhile f xs)
    | otherwise       = []

--myTakeWhile'        :: (a -> Bool) -> [a] -> [a]
--myTakeWhile' _ []   = []
--myTakeWhile' p xs =
--    foldr stepper [] xs
--    where
--        stepper x acc
--            | not p x   = 

notMyTakeWhile :: (a -> Bool) -> [a] -> [a]
notMyTakeWhile p ls = foldr step [] ls
    where
        step x acc
            | p x       = x : acc
            | otherwise = []
