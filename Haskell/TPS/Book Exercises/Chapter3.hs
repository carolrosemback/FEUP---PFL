import Data.List
-- LI7
deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = []
deleteOne v (x:xs) | v == x = xs
                   | otherwise = x: deleteOne v xs

-- LI8 
myConcat:: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs 

-- LI9
myReplicate ::  Int -> a  -> [a]
myReplicate 0 _ = []
myReplicate i v | i > 0 =  v: myReplicate (i-1) v
                | otherwise = error "negative number"

-- LI10
myCycle :: [a] -> [a]
myCycle [] = []
myCycle l = l ++ myCycle l

-- LI11
myIntersperse :: Char -> String -> String
myIntersperse _ [] = []
myIntersperse _ [x] = [x]
myIntersperse v (x:xs) = x : v : myIntersperse v xs

-- LI12
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) | n > 0 = x:myTake (n-1) xs
                | otherwise = error "negative number"

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop n (_:xs) | n > 0 = myDrop (n-1) xs
                | otherwise = error "negative number"


-- LI13
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt _ [] = ([],[])
mySplitAt 0 l = ([], l)
mySplitAt n (x:xs) | n > 0 = let (a,b) = mySplitAt (n-1) xs in (x:a,b)
                   | otherwise = error "negative number"

-- LI14
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup [x] = [[x]]
myGroup (x:y:xs) | x == y = (x : g) : gs
                 | otherwise = [x]:g:gs
                 where (g:gs) = myGroup (y:xs)

-- LI15
myInits :: String -> [String]
myInits [] = [[]]
myInits (x:xs) = [] : addHead x (myInits xs)

addHead :: a -> [[a]] -> [[a]]
addHead _ [] = []
addHead h (l:ls) = (h:l) : addHead h ls

-- LI16
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y): myZip xs ys

myZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myZip3  [] _ _  = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myZip3 xs ys zs


-- LI17
differentFromNext :: Eq a =>  [a] -> [a]
differentFromNext [] = []
differentFromNext [x] = []
differentFromNext (x:y:xs) | x /= y = x: differentFromNext (y:xs)
                           | otherwise = differentFromNext (y:xs)


-- LI18
myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose m = hs:myTranspose ts
                where (hs,ts) = splitHeadsTails m

splitHeadsTails :: [[a]] -> ([a], [[a]])
splitHeadsTails [] = ([],[])
splitHeadsTails (ys:xs) = case ys of [] -> (hs,ts)
                                     [z] -> (z:hs,ts)
                                     (z:zs) -> (z:hs, zs:ts)
                          where (hs,ts) = splitHeadsTails xs


-- LI19 
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub l = myNubAux l []

myNubAux :: Eq a => [a] -> [a] -> [a]
myNubAux [] _ = []
myNubAux (x:xs) uniques | x `elem` uniques = myNubAux xs uniques
                        | otherwise = x : myNubAux xs (x:uniques)             


-- LI20
mySubsequences :: [a] -> [[a]]
mySubsequences [] = [[]]
mySubsequences (x:xs) = addOrNotToHead x (mySubsequences xs)

addOrNotToHead :: a -> [[a]] -> [[a]]
addOrNotToHead h [] = []
addOrNotToHead h (l:ls) = l:(h:l): addOrNotToHead h ls 

-- LI27
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct [] [] = 0 
scalarProduct l1 l2 = sum [x*y | (x,y) <- zip l1 l2]

-- LI30
myConcat' :: [[a]] -> [a]
myConcat' [] =[]
myConcat' l = [x | xs <- l, x <- xs]

-- LI31
differentFromNext' ::Eq a => [a] -> [a]
differentFromNext' [] =[] 
differentFromNext' l = [x | (x,y) <- zip l (tail l), x /= y]

-- LI32
conseqPairs :: Eq a => [a] -> [(a,a)]
conseqPairs [] = []
conseqPairs l = [(x,y) | (x,y) <- zip l (tail l)]

-- LI33
myZip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
myZip3' [] _ _ = []
myZip3' _ [] _ = []
myZip3' _ _ [] = []
myZip3' l1 l2 l3 = [(x,y,z) | (x,(y,z)) <- zip l1 (zip l2 l3)]


-- LI35
checkMod3ThenOdd :: Integral a => [a] -> Bool 
checkMod3ThenOdd l = and [mod x 2 == 1  | x <- l, mod x 3 == 0]

-- LI36
repeatNTimes :: Integral a => [a] -> a -> [a]
repeatNTimes l n = [x | x <- l, _ <- [1..n]]

-- LI39
myPermutations :: Eq a => [a] -> [[a]]
myPermutations [] = [[]]
myPermutations l = [h:t | h <- l, t <- myPermutations(delete h l)]