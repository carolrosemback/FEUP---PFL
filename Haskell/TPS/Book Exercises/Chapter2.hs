
-- FT3
mySwap :: (a,b) -> (b,a)
mySwap (a,b) = (b,a)

-- FT4
distance2 :: Floating a => (a,a) -> (a,a) -> a
distance2 (a,b) (c,d) = sqrt ((a-c)^2 + (b-d)^2)

distance2' :: Floating a => (a,a) -> (a,a) -> a
distance2' (a,b) (c,d) = sqrt(sum1 + sum2)
                        where sum1 = (a-c)^2
                              sum2 = (b-d)^2

--distanceInf :: Floating a => (a,a) -> (a,a) -> a
distanceInf :: (Ord a, Num a) => (a, a) -> (a, a) -> a
distanceInf (x1,y1) (x2,y2) = max (abs(x1-x2)) (abs(y1-y2))

-- FT8
myMinimum :: Ord p => [p] -> p
myMinimum (x:xs) = myMinimumAux xs x
myMinimum []  = error "Empty"


myMinimumAux :: Ord t => [t] -> t -> t
myMinimumAux [] m = m
myMinimumAux (x:xs) m | x < m = myMinimumAux xs x
                      | otherwise  = myMinimumAux xs m


-- FT9
{- a) []
b) [[1,2],[],[3],[4,5]]
c) [[1,2],[],[3],[4,5]]
d) [4,5]
e) 3
f) error
g) [[1,2], [3,4,5]]
h) [[],[],[]]
i) ["abc","", "dce"]
j) [[]],[2],[3]]
k) 12
l) True -}

-- FT10
-- the function returns a pair with the third element of the input list 1
-- and the sublist of 1 starting at the fourth element 
f :: [a] -> (a,[a])
f (_:_:x:y) = (x,y)

f':: [a] -> (a,[a])
f' l = (l!!2, drop 3 l)

-- FT11
evaluateLength :: [a] -> [Char]
evaluateLength l | length l  <= 1 = "short"
                 | length l <= 3 = "medium-sized"
                 | otherwise  = "long"

evaluateLength' :: [a] -> [Char]
evaluateLength' [] = "short"
evaluateLength' [_] = "short"
evaluateLength' [_,_] = "medium-sized"
evaluateLength' [_,_,_] = "medium-sized"
evaluateLength' _ = "long" -- all other cases   

-- FT14
myScalarProduct :: Num p => [p] -> [p] -> p
myScalarProduct [] [] = 0
myScalarProduct (x:xs) (y:ys) = x*y + myScalarProduct xs ys

-- FT16
myElem :: Eq t => t -> [t] -> Bool
myElem _ [] = False
myElem y (x:xs)  | x==y = True
                 | otherwise = myElem y xs


-- FT 21
{- 1) [(Integer, Char)]
2) [[Int]]
3) [Char]
4) [Float]
5) Bool
6) [((Char,Char), Char)] -}
{- 
1) Num a =>[(a, Char)]
2) Num a =>[[a]]
3) [Char]
4) Fractional a =>[a]
5) Bool
6) [((Char,Char),Char)]
 -}

 -- FT 23
{-  a) Integer a => a -> a -> a
 b) (a,b) -> b 
 c) [[Int], Int]]
 d) [Int] -> [a] -> [a]
 e) [a] -> [Int] -> a
 f) [a] -> [b] -> [(a, b)]
 g) Floating a => [a] -> a -> a
 h) Ord a => [[a] -> Bool]
 i) a -> a -> b
 j) a -> b -> a -> c
 k) [[a] -> [a] -> [(a,a)]] -}

