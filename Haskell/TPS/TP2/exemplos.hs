divisores :: Int -> [Int]
divisores n = [x | x<-[1..n], mod n x==0];

testarPrimo :: Int -> Bool
testarPrimo n = divisores n == [1,n]

myReplicate :: Integral a => a->b -> [b]
myReplicate 0 _ = []
myReplicate n x | n > 0 = x:myReplicate (n-1) x
                | otherwise = error "argumento negativo"

myReplicate' :: Integral a => a->b -> [b]
myReplicate' n x = [x | _ <- [1..n]]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat l = [x | xs<-l , x<-xs]

(@@@) :: Integral a => [b] -> a -> b
(@@@) l n = head [x | (x,i) <- zip l [0..], i == n]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse'Aux :: [a] -> [a] -> [a]
myReverse'Aux [] acc = acc -- acc é um acumulador 
myReverse'Aux (x:xs) acc = myReverse'Aux xs (x:acc) -- ex: myReverse'Aux [1,2,3] [] -> myReverse'Aux [2,3] 1:[] -> myReverse'Aux [3] 2:[1] -> myReverse'Aux [] 3:[2,1] -> [3,2,1] 

myReverse' :: [a] -> [a]
myReverse' l = myReverse'Aux l []