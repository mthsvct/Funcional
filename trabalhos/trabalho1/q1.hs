module Q1_trab1 where

dividir(x, y) = div x y

pegaFator(x, y)
        | mod x y == 0 = y
        | otherwise = pegaFator(x, y+1)


fator(x)
        | x == 1 = []
        | otherwise = pegaFator(x, 2) : fator(dividir(x, pegaFator(x, 2)))

apresenta(x, f) = do
        putStrLn("Fator de " ++ show x ++ ":")
        putStrLn(show(f))

fatoracao(a, b, c) = do
        apresenta(a, fator(a))
        apresenta(b, fator(b))
        apresenta(c, fator(c))