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

convex :: Shape -> Bool
convex (Polygon (v1:vs)) = let vs' = (v1:vs) ++ [v1]
                           in isPolyConvex vs'

isPolyConvex :: [Vertex] -> Bool
isPolyConvex (v1:v2:v3:vs) = ((crossProduct v1 v2 v3) > 0) && (isPolyConvex (v2:v3:vs))
isPolyConvex (v1:v2:[]) = True

crossProduct :: Vertex -> Vertex -> Vertex -> Float
crossProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)


main = (print (convex (Polygon [(0, 0), (5, 0), (5, 5), (0, 5), (2, 2)])))