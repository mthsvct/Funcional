soma(x, i)
        | i < length x = (x !! i) + soma(x, i+1)
        | otherwise = 0

produto(x, i)
        | i < length x = (x !! i) * produto(x, i+1)
        | otherwise = 1

-- c:r referece a c ser a cabeça da lista e r ser o resto da lista.
somaMult3([], soma) = soma
somaMult3(c:r, soma)
        | mod c 3 == 0 = somaMult3(r, soma + c)
        | otherwise = somaMult3(r, soma)

geraLista(i, limite)
        | i < limite = i : geraLista(i+1, limite)
        | otherwise = []

geraListaInvertida(i, limite)
        | i < limite = geraListaInvertida(i+1, limite) ++ [i]
        | otherwise = []

main = do
        putStrLn "Hello World!"