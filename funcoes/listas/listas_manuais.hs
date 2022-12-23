soma(x, i)
        | i < length x = (x !! i) + soma(x, i+1)
        | otherwise = 0

produto(x, i)
        | i < length x = (x !! i) * produto(x, i+1)
        | otherwise = 1

geraLista(i, limite)
        | i < limite = i : geraLista(i+1, limite)
        | otherwise = []

geraListaInvertida(i, limite)
        | i < limite = geraListaInvertida(i+1, limite) ++ [i]
        | otherwise = []

main = do
        putStrLn "Hello World!"