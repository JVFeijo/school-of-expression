
inverseAppend :: [a] -> [a] -> [a]
inverseAppend xs [] = xs
inverseAppend xs (y:ys) = y : (inverseAppend xs ys)

reverseSlow :: [a] -> [a]
reverseSlow (x:[]) = [x]
reverseSlow (x:xs) = (reverseSlow xs) ++ [x] -- O(n**2)

reverseFast :: [a] -> [a]
reverseFast xs = rev [] xs -- O(n)
            where
                rev acc [] = acc
                rev acc (x:xs) = rev (x:acc) xs

main = (print (reverseFast "hello world"))