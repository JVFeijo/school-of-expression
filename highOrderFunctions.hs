type Vertex = (Float, Float)
type Triangle = (Vertex, Vertex, Vertex)

reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl (\acc a -> a : acc) [] xs

area :: [Vertex] -> Float
area vs = let triangles = divideInTriangles vs
              trianglesAreas = map triArea triangles
              polygonArea = foldl (+) 0 trianglesAreas
            in polygonArea

divideInTriangles :: [Vertex] -> [(Vertex, Vertex, Vertex)]
divideInTriangles (v1:v2:vs) = let (triangles, _) = foldl (\(acc, previous) curr -> ((v1, previous, curr) : acc, curr)) ([], v2) vs
                                in triangles

triArea :: Triangle -> Float
triArea (v1, v2, v3) = let a = distBetween v1 v2
                           b = distBetween v2 v3
                           c = distBetween v3 v1
                           s = 0.5 * (a + b + c)
                        in  sqrt (s * (s - a) * (s - b) * (s - c))

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

main = (print (area [(7,0),(4,0),(4,5),(0,5)]))