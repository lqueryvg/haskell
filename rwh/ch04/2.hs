
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate [] = []
splitWith predicate xs
    | null wanted_sublist = split_the_rest
    | otherwise           = wanted_sublist : split_the_rest
    where
        (wanted_sublist, the_rest)     = break predicate xs
        split_the_rest = splitWith predicate (dropWhile predicate the_rest)

-- splitWith f xs =
--     case pre of
--         [] -> split_the_rest
--         _  -> pre : split_the_rest
--     where
--         (pre, suf)     = break f xs
--         split_the_rest = splitWith f (dropWhile f suf)

