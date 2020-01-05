
reverseFoldl :: [a] -> [a]
reverseFoldl xs = foldl (\acc a -> a : acc) [] xs

main = (print (reverseFoldr "hello world"))