primos :: [Int]
primos = crivo [2..]

crivo :: [Int] -> [Int]
crivo (p:xs) = p : crivo [x | x<-xs, mod x p /=0]  -- encontra primos 

factores :: Int -> [Int]
factores a = snd (until (\(x,y) -> x == 1) (\(x,y) -> (div x (primeiroDivisorPrimo x 2), y ++ [primeiroDivisorPrimo x 2])) (a,[]))

primeiroDivisorPrimo :: Int -> Int -> Int
primeiroDivisorPrimo a i    | mod a (head (crivo [i..])) == 0 = head (crivo [i..]) -- divide pelos primos na lista atraves de crivo
                            | otherwise = primeiroDivisorPrimo a (i+1)



-- snd: returns the second item in a tuple
-- until: applies a function which is passed as the second argument to the third argument and it compares the result with the condition, 
        --if the condition evaluates to True, it prints the result, if not, it passes the result to the finction 
        --and repeats the cycle as long as the condition is matched