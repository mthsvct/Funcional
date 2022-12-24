{- 
    Faça uma função em Haskell com pendência que dado um 
    intervalo devolva a quantidade de múltiplos de 7
    que não são múltiplos de 2 e de 5.
-}

compara(i)
        | mod i 7 == 0 && mod i 2 /= 0 && mod i 5 /= 0 = 1
        | otherwise = 0

multiplos(i, f)
        | i < f = compara(i) + multiplos(i+1, f)
        | i == f = 0

main = do
        putStrLn "Hello World!"
