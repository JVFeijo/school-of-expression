putCharList :: String -> [IO ()]
putCharList [] = []
putCharList (c:cs) = putChar c : putCharList cs

putStr2 :: String -> IO ()
putStr2 [] = return ()
putStr2 (c:cs) = do putChar c
                    putStr2 cs

getLine2 :: IO String
getLine2 = do c <- getChar
              if c == '\n' then return ""
              else do l <- getLine2
                      return (c : l)


main = (do str <- getLine2
           print str)