
-- IN8
five :: (Eq a, Num a) => a -> [Char]
five x | x == 5 = "five"
       | otherwise = "not five"

-- IN9
min3 :: Ord a => a -> a -> a -> a
min3 x y z
  | x < y && x < z = x
  | y < z = y
  | otherwise = z

min3' :: Ord a => a -> a -> a -> a
min3' x y z = minimum [x, y,z]

-- IN13
f':: (Ord a, Num a, Integral b) => a -> b
f' 0 = 0
f' x = if x > 0
        then 1
        else -1

-- IN14
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial x = x * factorial (x-1)


-- IN15
myGcd :: Integral  a => a -> a -> a
myGcd a b = myGcdAux (max posA posB) (min posA posB)
            where posA = abs a
                  posB = abs b

myGcdAux :: Integral a => a -> a -> a
myGcdAux a 0 = a
myGcdAux a b = myGcdAux b (mod a b)


-- IN16
mPower :: (Ord t, Fractional a, Num t) => a -> t -> a
mPower m n | n == 0 = 1
           | n > 0  = m * mPower m (n-1)
           | otherwise = 1 / mPower m (-n)

-- IN20
isPrime :: Integral t => t -> Bool
isPrime x | x == 2 = True
          | x <= 0 = False
          | even x = False -- all prime numbers are odd, excluding 2
          | otherwise = not (hasPrimeFactor 3 x)

-- check if a number has a prime divisor. iterator i checks if i divides n
-- returns False if the iterator surpasses sqrt(n)
-- uses i*i > n instead of n > sqrt(n) as its more efficient    
hasPrimeFactor :: Integral t => t -> t -> Bool
hasPrimeFactor i n | i*i > n = False
               | n `mod` i == 0 = True
               | otherwise  = hasPrimeFactor (i+2) n