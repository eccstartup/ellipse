module Ellipse (circumference, area) where

meanag (x, y)
  | near2 (x, y) (ag (x, y)) = (x, y)
  | otherwise = meanag (ag (x, y))
  where near2 (a, c) (b, d) = sqrt ((a - b)^2 + (c - d)^2) < 10**(-12)
        ag (x, y) = ((x+y)/2, sqrt(x*y))

meanmag (x, y, z)
  | near3 (x, y, z) (mag (x, y, z)) = (x, y, z)
  | otherwise = meanmag (mag (x, y, z))
  where near3 (a, c, e) (b, d, f) = sqrt ((a - b)^2 + (c - d)^2) < 10**(-12)
        mag (x, y, z) = ((x+y)/2, z + sqrt((x-z)*(y-z)), z - sqrt((x-z)*(y-z)))

circumference a b = 2 * pi * f (meanmag (a^2, b^2, 0)) / (fst (meanag (a, b)))
     where f (a, b, c) = a

area a b = pi * a * b
