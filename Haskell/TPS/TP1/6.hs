
raizes :: Float -> Float -> Float -> (Maybe Float, Maybe Float)
raizes a b c | delta < 0 = (Nothing, Nothing)
             | delta == 0 = (Just (-b/(2*a)), Nothing) 
             | otherwise = let x = sqrt delta in (Just ((-b + x) / (2*a)), Just ((-b -x) / (2*a))) 
             where delta  = (b^2) - (4*a*c)


raizes2 :: Float -> Float -> Float -> [Float]
raizes2 a b c | delta > 0 = [(-b + sqrt delta)/(2*a),(-b -sqrt delta)/(2*a)]
              | delta == 0 = [-b/(2*a)]
              | otherwise = []
               where delta = b^2 - 4*a*c