calcula(a, b, c, d, i)
        | i == 1000 = -1
        | mod i a == 0 && mod i b == 0 && mod i c == 0 && mod i d == 0 = i
        | otherwise = calcula(a, b, c, d, i+1)

apresenta(a, b, c, d, m)
        | m == -1 = putStrLn("Nao existe MMC para os numeros informados")
        | otherwise = do
                putStr("O MMC de ")
                putStr(show(a))
                putStr(", ")
                putStr(show(b))
                putStr(", ")
                putStr(show(c))
                putStr(" e ")
                putStr(show(d))
                putStr(" eh: ")
                putStrLn(show(m))

mmc :: (Int, Int, Int, Int) -> IO ()
mmc(a, b, c, d) 
        | a < b = mmc(b, a, c, d)
        | b < c = mmc(a, c, b, d)
        | c < d = mmc(a, b, d, c)
        | otherwise = apresenta(a, b, c, d, calcula(a, b, c, d, d+1))

-- Falta fazer a troca.

main = do
        putStrLn "Hello World!"