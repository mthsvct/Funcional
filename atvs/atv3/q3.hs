
calcula :: (Int, Int, Int) -> Int
calcula(a, b, i)
        | i == 100 = -1
        | mod i a == 0 && mod i b == 0 = i
        | otherwise = calcula(a, b, i+1)

apresenta(a, b, m) = do
        putStrLn("O MMC de " ++ show(a) " e " ++ show(b) " eh: " ++ show(m))

mmc :: (Int, Int) -> IO ()
mmc(a, b)
        | a > b = mmc(b, a)
        | otherwise = apresenta(a, b, calcula(a, b, b))

main = do
        putStrLn "Hello World!"