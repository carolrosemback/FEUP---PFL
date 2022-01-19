mediana :: Ord a => a -> a -> a -> a
mediana x y z | (x <= y && x >= z) || (x >= y && x <= z) = x
              | (y <= x && y >= z) || (y >= x && y <= z) = y
              | otherwise = z