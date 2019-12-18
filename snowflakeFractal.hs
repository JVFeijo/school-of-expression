import Graphics.SOE

mulcos30 :: Int -> Int
mulcos30 n = n * 86603 `div` 100000

minSize :: Int
minSize = 8

spaceClose :: Window -> IO ()
spaceClose w = do
                    k <- getKey w
                    if k == ' ' then closeWindow w
                                else spaceClose w

 
equiTri :: Color -> Window -> Int -> Int -> Int -> Int -> IO ()
equiTri color w x y yorient size
    = let halfsize = size `div` 2
          height = mulcos30 size
          a = (x, y)
          b = (x + halfsize, y - (height * yorient))
          c = (x + size, y)
      in drawInWindow w (withColor color
                         (polygon [a, b, c, a]))
 
defaultColors :: [Color]
defaultColors = [Blue, Green, Red, Cyan, Magenta, Yellow]
 
starOfDavid :: [Color] -> Window -> Int -> Int -> Int -> IO ()
starOfDavid (c:cs) w x y size
    = let twothirdsheight = 2 * (mulcos30 size) `div` 3
          fourninthsheight = 4 * (mulcos30 size) `div` 9
          twoninthsheight = fourninthsheight `div` 2
          onethirdsize = size `div` 3
      in do equiTri c w x y 1 size
            equiTri c w x (y-twothirdsheight) (-1) size
            if size >= minSize
             then do starOfDavid cs w x y onethirdsize
                     starOfDavid cs w (x + onethirdsize * 2)
                                 y onethirdsize
                     starOfDavid cs w (x + onethirdsize)
                                 (y - twothirdsheight) onethirdsize
                     starOfDavid cs w (x + onethirdsize * 2)
                                 (y - fourninthsheight) onethirdsize
                     starOfDavid cs w x (y - fourninthsheight) onethirdsize
                     starOfDavid cs w (x + onethirdsize)
                                 (y + twoninthsheight) onethirdsize
             else return ()
starOfDavid [] w x y size = starOfDavid defaultColors w x y size
 
main = runGraphics (
                    do w <- openWindow "Snowflake Fractal" (1000,1000)
                       starOfDavid [] w 250 500 500
                       spaceClose w
                   )