data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
 deriving Show

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

rectangle :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [v1, v2, v3, v4]
                 where
                    v1 = (s1/2, s2/2)
                    v2 = (-s1/2, s2/2)
                    v3 = (-s1/2, -s2/2)
                    v4 = (s1/2, -s2/2)

rtTriangle :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [v1, v2, v3]
                  where
                     v1 = (-s1/2, s2/2)
                     v2 = (-s1/2, -s2/2)
                     v3 = (s1/2, -s2/2)

regularPolygon :: Int -> Side -> Shape
regularPolygon n s = Polygon (regularVerts (fromIntegral n) s (fromIntegral n))

regularVerts :: Float -> Float -> Float -> [(Float, Float)]
regularVerts _ _ 0 = []
regularVerts n s i = 
                     let 
                        angle = 2 * pi * (n-i) / n
                        radius = s / (2 * (sin (pi / n)))
                        x = radius * cos angle
                        y = radius * sin angle
                     in  (x, y):(regularVerts n s (i-1))

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (RtTriangle s1 s2) = s1 * s2/2
area (Ellipse r1 r2) = pi * r1 * r2
area (Polygon (v1:vs)) = polyArea vs
                        where 
                           polyArea :: [Vertex] -> Float
                           polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea(v3:vs')
                           polyArea _ = 0

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = 0.5 * (a + b + c)
                   in  sqrt (s * (s - a) * (s - b) * (s - c))

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

main = (print (area (Polygon [(0, 0), (3.652, 0), (0, 5.126)])))