data Tree a = Leaf a | Branch a (Tree a) (Tree a)
              deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf v) = Leaf (f v)
mapTree f (Branch v t1 t2) = Branch (f v) (mapTree f t1) (mapTree f t2)

foldrTree :: (b -> a -> b) -> b -> Tree a -> b
foldrTree f acc (Leaf v) = f acc v
foldrTree f acc (Branch v t1 t2) = f (foldrTree f (foldrTree f acc t2) t1) v

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree f acc (Leaf a) = f acc a
foldlTree f acc (Branch v t1 t2) = f (foldlTree f (foldlTree f acc t1) t2) v

foldBfsTree :: (b -> a -> b) -> b -> Tree a -> b
foldBfsTree f acc t = foldl f acc bfsArray
  where bfsArray = zipTree t

zipTree :: Tree a -> [a]
zipTree (Leaf v) = [v]
zipTree (Branch v t1 t2) = v:(concat (zipWith (\x y -> x:y:[]) (zipTree t1) (zipTree t2)))
 
foldBfsTree2 :: (b -> a -> b) -> b -> Tree a -> b
foldBfsTree2 f acc tree = aux f acc [tree]

aux :: (b -> a -> b) -> b -> [Tree a] -> b
aux f acc ((Leaf x):[]) = f acc x
aux f acc ((Leaf x):ts) = f (aux f acc ts) x
aux f acc ((Branch x t1 t2):ts) = f (aux f acc (ts ++ [t1, t2])) x

root :: Tree a -> a
root (Leaf x) = x
root (Branch x t1 t2) = x

t = Branch 7 (Branch 5 (Leaf 1) (Leaf 2)) (Branch 6 (Leaf 3) (Leaf 4))

main = (print (foldBfsTree2 (\acc x -> x:acc) [] t))