type Vertex = (Float, Float)

reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl (\acc a -> a : acc) [] xs

-- area :: [Vertex] -> Float
area (v1:v2:vs) = let (answer, _) = foldl (\(acc, previous) curr -> ((triArea v1 previous curr + acc), curr)) (0, v2) vs
                    in answer

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = let a = distBetween v1 v2
                       b = distBetween v2 v3
                       c = distBetween v3 v1
                       s = 0.5 * (a + b + c)
                     in  sqrt (s * (s - a) * (s - b) * (s - c))

distBetween :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

main = (print (area [(7,0),(4,0),(4,5),(0,5)]))