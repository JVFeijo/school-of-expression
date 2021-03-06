import Shape

data Region = Shape Shape 
  | Translate Vector Region
  | Scale Vector Region
  | Complement Region
  | Region `Union` Region
  | Region `Intersect` Region
  | HalfPlane Ray
  | Empty
  deriving Show

type Vector = (Float, Float)

infix 5 `Union`
infix 6 `Intersect`

type Coordinate = (Float, Float)
type Ray = (Coordinate, Coordinate)

isLeftOf :: Coordinate -> Ray -> Bool
(px, py) `isLeftOf` ((ax, ay), (bx, by)) =
                                            let (s, t) = (px - ax, py - ay)
                                                (u, v) = (px - bx, py - by)
                                            in  s * v >= t * u

isRightOf :: Coordinate -> Ray -> Bool
(px, py) `isRightOf` ((ax, ay), (bx, by)) =
                                            let (s, t) = (px - ax, py - ay)
                                                (u, v) = (px - bx, py - by)
                                            in  s * v <= t * u

containsS :: Shape -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x, y) = 
                                        let t1 = s1/2
                                            t2 = s2/2
                                        in  -t1 <= x && x <= t1 && -t2 <= y && y <= t2
(Ellipse r1 r2) `containsS` (x, y) =
  (x/r1)**2 + (y/r2)**2 <= 1
(Polygon pts) `containsS` p =
                              let leftOfList = map isLeftOfP (zip pts (tail pts ++ [head pts]))
                                  isLeftOfP p' = isLeftOf p p'
                                  rightOfList = map isRightOfP (zip pts (tail pts ++ [head pts]))
                                  isRightOfP p' = isRightOf p p'
                              in  and leftOfList || and rightOfList
(RtTriangle s1 s2) `containsS` p =
                                  (Polygon [(0, 0), (s1, 0), (0, s2)]) `containsS` p

containsR :: Region -> Coordinate -> Bool
(Shape s) `containsR` p = s `containsS` p
(Translate (w, v) r) `containsR` (x, y) = r `containsR` (x - w, y - v)
(Scale (w, v) r) `containsR` (x, y) = r `containsR` (x / w, y / v)
(Complement r) `containsR` p = not (r `containsR` p)
Empty `containsR` p = False
(rl `Union` r2) `containsR` p = rl `containsR` p || r2 `containsR` p
(rl `Intersect` r2) `containsR` p = rl `containsR` p && r2 `containsR` p
(HalfPlane ray) `containsR` p = p `isLeftOf` ray


annulus :: Radius -> Radius -> Region
annulus r1 r2 = (Complement (Shape (Ellipse r1 r1))) `Intersect` (Shape (Ellipse r2 r2))

halfPlanePolygon :: [Coordinate] -> Region
halfPlanePolygon (cd1:cdx) = makeHalfPlanePolygon cdx
                   where
                     makeHalfPlanePolygon :: [Coordinate] -> Region
                     makeHalfPlanePolygon (cd2:cd3:cdx') = (HalfPlane (cd2, cd3)) `Intersect` (makeHalfPlanePolygon cdx')
                     makeHalfPlanePolygon (cd2:cd3:[]) = HalfPlane (cd2, cd3) `Intersect` (HalfPlane (cd3, cd1))

flipX :: Region -> Region
flipX r = Scale (-1, 1) r

flipY :: Region -> Region
flipY r = Scale (1, -1) r