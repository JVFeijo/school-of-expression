
inverseAppend :: [a] -> [a] -> [a]
inverseAppend xs [] = xs
inverseAppend xs (y:ys) = y : (inverseAppend xs ys)

reverseStr :: String -> String
reverseStr (c:[]) = [c]
reverseStr (c:cs) = (reverseStr cs) ++ [c]

main = (print (reverseStr "hello world"))