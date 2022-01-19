last1 :: [a] -> a
last1 x = head (reverse x)

init1 :: [a] -> [a] 
init1 x = take (length x - 1) x 