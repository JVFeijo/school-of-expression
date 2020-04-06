data Tree a = Leaf a | Branch a (Tree a) (Tree a)
              deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf value) = Leaf (f value)
mapTree f (Branch value t1 t2) = Branch (f value) (mapTree f t1) (mapTree f t2)

foldrTree :: (b -> a -> b) -> b -> Tree a -> b
foldrTree f acc (Leaf a) = f acc a
foldrTree f acc (Branch value t1 t2) = f (foldTree f (foldTree f acc t2) t1) value

t = Branch 3 (Leaf 1) (Leaf 2)

main = (print (foldTree (\acc value -> value:acc) [] t))