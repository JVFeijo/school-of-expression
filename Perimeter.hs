module Perimeter (perimeter,
                  module Shape
                 ) where
import Shape

epsilon = 0.0001 :: Float

perimeter :: Shape -> Float
perimeter (Rectangle s1 s2)   = 2 * (s1 + s2)
perimeter (RtTriangle s1 s2)  = s1 + s2 + sqrt (s1^2 + s2^2)
perimeter (Polygon vs)        = foldl (+) 0 (sides vs)
perimeter (Ellipse r1 r2)
    | r1 > r2 = ellipsePerim r1 r2
    | otherwise = ellipsePerim r2 r1
  where ellipsePerim r1 r2 = let e = sqrt (r1^2 - r2^2) / r1
                                 s = scanl aux (0.25 * e^2) [2..]
                                 aux s i = nextEl e s i
                                 test x  = x > epsilon
                                 sSum    = foldl (+) 0 (takeWhile test s)
                                in 2 * r1 * pi * (1 - sSum)

nextEl :: Float -> Float -> Float -> Float
nextEl e s i = s * (2 * i - 1) * (2 * i - 3) * (e^2) / (4 * i^2)

sides :: [Vertex] -> [Float]
sides vs = zipWith distBetween vs (tail vs ++ [head vs])

main = (print (perimeter (Polygon [(4.0, 0.0), (4.0, 4.0), (0.0, 4.0), (0.0, 0.0)])))