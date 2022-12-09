{- 
    Faça  uma  função  em  Haskell  que  dado  um  intervalo  
    devolva  a  quantidade  de  múltiplos  de  3  que  não  são
    múltiplos de 2 e de 5.
-}

verificaMultiplos(x) | (mod x 3 == 0) && (mod x 2 /= 0) && (mod x 5 /= 0) = True
                | otherwise = False

calcula(x, y) | x < y && verificaMultiplos(x) == True = 1 + calcula(x+1, y)
                | x < y = 0 + calcula(x+1, y)
                | otherwise = 0

main = do
    putStrLn "Hello World!"