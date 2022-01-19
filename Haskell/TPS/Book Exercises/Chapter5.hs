

-- UT1 
type HashMap k v = [(k,v)]

-- a
type IntMap v = HashMap Int v

-- b
mapInt :: IntMap Char
mapInt = [(1,'a'), (2,'b')]

mapString :: HashMap String Bool
mapString = [("Ana", True), ("John", False)]

-- c 
member :: Eq k => k -> HashMap k v -> Bool
member _ [] = False
member x ((k,v):kvs) | x == k = True
                     | otherwise = member x kvs
-- d
keySum :: IntMap v -> Int
keySum [] = 0
keySum ((k,v):kvs) = k + keySum kvs


-- UT3
type Pair a = (a,a)
type Relation a = [Pair a]

isReflexive :: Eq a => Relation a -> Bool
isReflexive r = isReflexiveAux r r

isReflexiveAux:: Eq a => [Pair a] -> Relation a -> Bool
isReflexiveAux [] _ = True
isReflexiveAux ((x,y):xs) r | (y,x) `elem` r = isReflexiveAux xs r
                            | otherwise = False

isTransitive :: Eq a => Relation a -> Bool
isTransitive r = isTransitiveAux [(x,y) | x <- r, y <- r] r

isTransitiveAux :: Eq a => [Pair (Pair a)] -> Relation a -> Bool
isTransitiveAux [] _ = True
isTransitiveAux (((x,y),(w,z)):xs) r | y == w && notElem (x,z) r = False
                                   | otherwise = isTransitiveAux xs r


-- UT6
data Shape = Circle Double Double Double | Rectangle Double Double Double Double

perimeter :: Shape -> Double
perimeter (Circle _ _ r ) = 2*pi*r
perimeter (Rectangle x1 y1 x2 y2) = 2*abs(x1-x2) + 2*abs(y1-y2)

-- UT7
lookUp:: Eq k => k -> HashMap k v -> Maybe v
lookUp _ [] = Nothing
lookUp x ((k,v):kvs) | x == k = Just v
                     | otherwise = lookUp x kvs

areEqual :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
areEqual m1 m2 = (length m1 == length m2) && isContainedIn m1 m2


isContainedIn :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
isContainedIn [] _ = True
isContainedIn ((k,v):kvs) m2 = case lookUp k m2 of (Just v2) -> v == v2 && isContainedIn kvs m2
                                                   otherwise -> False


ceilingKey :: (Eq k, Ord k) => k -> HashMap k v -> Maybe k
ceilingKey k m = ceilingKeyAux k m Nothing

ceilingKeyAux :: (Eq k, Ord k) => k -> HashMap k v -> Maybe k -> Maybe k
ceilingKeyAux _ [] acc = acc
ceilingKeyAux k ((k1,v1):kvs) Nothing | k1 >= k = ceilingKeyAux k kvs (Just k1)
                                      | otherwise = ceilingKeyAux k kvs Nothing
ceilingKeyAux k ((k1,v1):kvs) (Just kc) | k1 >= k && k1 < kc = ceilingKeyAux k kvs (Just k1)
                                        | otherwise = ceilingKeyAux k kvs (Just kc)



-- UT8
data NestedList a = Elem a | List [NestedList a] -- pode ser apenas um elemento ou uma lista de elementos

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- UT9
data Country  = Ct {name::String, 
                    population::Int, 
                    surfaceArea::Double, 
                    continent::String } deriving(Show, Eq)

populationDensity :: Country -> Double
populationDensity c = fromIntegral(population c) /surfaceArea c

countContinent :: String -> [Country] -> Int 
countContinent cont l = length (filter( == cont) (map continent l))


