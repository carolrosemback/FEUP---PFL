curta :: [a] -> Bool
curta x = length x < 3

-----------------------

curta2 :: [a] -> Bool 
curta2 [] = True 
curta2 [_] = True 
curta2 [_,_] = True 
curta2 x = False 