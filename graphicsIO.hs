import Graphics.SOE

spaceClose :: Window -> IO ()
spaceClose w = do
                    k <- getKey w
                    if k == ' ' then closeWindow w
                                else spaceClose w

main = runGraphics (
    do
        w <- openWindow "My First Graphics Program" (300, 300)
        drawInWindow w (text (100, 200) "HelloGraphics World")
        spaceClose w
    )