pitagoricos :: Integral a => a -> [(a,a,a)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]

pitagoricos2 :: Integer -> [(Integer, Integer, Integer)]
pitagoricos2 n = concat [ [(x,y,z),(y,x,z)] | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]