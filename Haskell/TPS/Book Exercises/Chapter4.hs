{-# LANGUAGE BlockArguments #-}
import Data.List
--import Data.List.Utils 

-- HO3
applyN :: Integral n => (a -> a) -> n -> a -> a
applyN _ 0 x = x
applyN f n x | n > 0 = f (applyN f (n-1) x)
             | otherwise = error "negative number"

cipher :: Integral n => n -> String -> String
cipher _ [] = []
cipher n (x:xs) = applyN nextChar n x:cipher n xs

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar c = succ c


-- HO4
--(a -> b -> b) -> b -> a -> b
f g a b = g a (g a b)
-- f recebe como input uma função g a qual o primeiro argmento deve ser do mesmo tipo
-- que o terceiro argumento de f (a) e o segundo argumento deve ser do mesmo tipo que 
-- o segundo argumento de f (b)
-- ao usar gab no segundo argumento da chamada de g garante que o output de g é do mesmo 
-- tipo que seu argumento, que por sua vez garante que o tipo do output de f é o mesmo que b


-- HO7
sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond (x:xs) cmp = sortByCond lower cmp ++ [x] ++ sortByCond upper cmp
                        where (lower , upper) = myPartition x xs cmp


myPartition :: Ord a => a -> [a] -> (a -> a -> Bool) -> ([a], [a])
myPartition _ [] _ = ([],[])
myPartition v (x:xs) cmp | cmp x v = (x:a,b)
                         | otherwise = (a,x:b)
                        where (a,b) = myPartition v xs cmp

-- HO8
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

-- HO9
myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f a b = f (a,b)

-- HO13
{- a) [[1,1,1],[2,2,2],[3,3,3]]
b) [0,0,1,1,1,2,2,2,3,3]
c) [4,16,36..]
d) 220
e) [53.0, 21.5, 11.0, 5.75, 2.6]
f) [1,3,9,27,81] -- potencias de 3 menores que 200
g) 26 -- numero de letras no alfabeto -}

-- HO14
orderedTriples :: Ord a => [(a,a,a)] -> [(a,a,a)]
orderedTriples = filter (\(a,b,c) -> a <=b && b <=c)

-- HO15
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs

-- HO16
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) | f x = x: myFilter f xs
                  | otherwise = myFilter f xs


-- HO17
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs) | f x = x: myTakeWhile f xs
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) | p x = myDropWhile p xs
                     | otherwise = x:xs

-- HO18
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll p (x:xs) = p x && myAll p xs

myAll' :: (a -> Bool) -> [a] -> Bool
myAll' = all

myAll'' :: (a -> Bool) -> [a] -> Bool
myAll'' p l = all p l


-- HO19
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:[f prev | prev <- myIterate f x]


-- HO20
myZipWith :: (a -> b-> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
myZipWith' f = zipWith (f)

-- HO22
countVowels :: String -> Int
countVowels []  = 0
countVowels l = length $ filter (\x -> x == 'a' || x == 'e'  || x == 'i' || x == 'o' || x == 'u' ) l

-- HO23
-- computa qual a maior ocorrencia do mesmo valor entre duas listas 

-- HO29
{- a) True
b) [6,20,64,2]
c) [[4,5,6], [12,11,10]] -- mantem sublistas do input na qual o terceiro elemento é par. even é composto com !!2
d) [8,8] -- filter even é composto com !!2 
 -}

 -- HO30
myReverse'  :: [a] -> [a]
myReverse' = foldr(\x acc -> acc ++ [x]) []

-- HO31
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x acc -> if f x then x:acc else acc) []
-- geralmente são usadas right-folds quando o output é uma lista, já que elas mantem os elemento da lista do input ordenados
-- left-folds são mais convenientes para inverter a ordem de uma lista 


-- HO32
myMap' :: (a -> b) -> [a] -> [b]
myMap' _ [] = []
myMap' f l = map f l

-- HO33
largePairs :: (Ord a, Num a) => a -> [(a,a)] -> [(a,a)]
largePairs m = foldr (\(x,y) acc -> if x+y > m then (x,y):acc else acc) []


-- HO35
separateSingleDigits :: Integral a => [a] -> ([a], [a])
separateSingleDigits = foldr(\x (ys,ns) -> if x >= 0 && x <= 9 then (x:ys, ns) else (ys, x:ns)) ([],[])


-- HO37
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ a [] = a
myFoldr f a (x:xs) =  f x (myFoldr f a xs)


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- HO40 
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr _ acc [] = [acc]
myScanr f acc (x:xs) = f x (head t):t
                    where t = myScanr f acc xs

myScanr' :: (a -> b -> b) -> b -> [a] -> [b]  
myScanr' _ acc [] = [acc]
myScanr' f acc l = foldr (\x (h:t) -> (f x h): h:t) [acc] l              


-- HO42
myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' f acc l = foldr (\x accF z -> f (accF z) x) id l acc 


-- HO43
kadane :: (Ord a, Num a) => [a] -> a
kadane = maximum . scanl (\acc x -> max x (acc + x))  0
-- scanl é usada para produzir o array A descrito no enunciado. Foi usado o left scan para processor os elementos iniciando pelo primeiro

-- HO45
myElem :: Eq a => a -> [a] -> Bool 
myElem = any . (==) 

-- HO47
--f :: [a] -> [a] -> [a]
--f xs ys = foldr (\x acc -> x:acc) ys xs

f' :: [a] -> [a] -> [a]
f' = flip (foldr (:))

-- HO48
myLast :: [a] -> a
myLast = head . reverse 

-- HO49
countLetters :: [Char] -> Int 
countLetters = length . concat . words 

countFirst :: [Char] -> Int 
countFirst = length . head . words  

-- HO50
myReverse'' :: [a] -> [a]
myReverse'' = foldl (\acc x -> x:acc) []

myReverse''' :: [a] -> [a]
myReverse''' = foldl (flip(:)) []

-- HO51
mySum :: Num a => [a] -> a 
mySum = foldl (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldl (*) 1 
 
myLength :: Integral b => [a] -> b
myLength = sum . map (const 1)

-- HO52
extractDigits :: Integer -> [Integer]
extractDigits = reverse . map (`mod` 10) . takeWhile(>0) . iterate (`div` 10)