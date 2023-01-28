
{-
    Faça uma função em Haskell com pendência que
    dado um intervalo devolva o produto entre a soma dos
    múltiplos de 5 e o cubo da soma dos divisores de 50.
-}

somaMultiplos(i, f)
        | i == f = 0
        | mod i 5 == 0 = i + somaMultiplos(i+1, f)
        | otherwise = 0 + somaMultiplos(i+1, f)
    
somaDivisores(i, f)
        | i == f = 0
        | mod 50 i == 0 = i + somaDivisores(i+1, f)
        | otherwise = 0 + somaDivisores(i+1, f)

produto(i, f) = somaMultiplos(i, f) * somaDivisores(i, f)

main = do
        putStrLn "Hello World!"