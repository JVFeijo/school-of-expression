putCharList :: String -> [IO ()]
putCharList [] = []
putCharList (c:cs) = putChar c : putCharList cs

putStr2 :: String -> IO ()
putStr2 (c:cs) = do putChar c
                    putStr2 cs

main = (putStr "joao")