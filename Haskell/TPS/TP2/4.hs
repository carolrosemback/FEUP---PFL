divdrop :: Integer -> [Integer]
divdrop n = [x | x<-[1..n], mod n x==0  && x /= n];