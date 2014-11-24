
-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead xs = Just $ head xs
-- 
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail xs = Just $ tail xs
-- 
-- safeLast :: [a] -> Maybe a
-- safeLast [] = Nothing
-- safeLast xs = Just $ last xs
-- 
-- safeInit :: [a] -> Maybe [a]
-- safeInit [] = Nothing
-- safeInit xs = Just $ init xs

safeListFunction f [] = Nothing
safeListFunction f xs = Just $ f xs

safeHead = safeListFunction head
safeTail = safeListFunction tail
safeLast = safeListFunction last
safeInit = safeListFunction init
