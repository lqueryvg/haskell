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

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "empty string"
asInt_either xs@(x:xs')

    | x == '-'        = Right ((-1) * (parse_digits xs'))
    | not (isDigit x) = Left ("non-digit'" ++ [x] ++ "'")
    | otherwise       = Right (parse_digits xs)

    where
        parse_digits str  = foldr f 0 str
        f tot c = (tot * 10) + (digitToInt c)