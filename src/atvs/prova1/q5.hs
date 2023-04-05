
produtorio(c, produto)
    | mod c 2 /= 0 = produto * c
    | otherwise = produto

somatorio(c, soma)
    | mod c 2 == 0 = soma + c
    | otherwise = soma

calcular([], soma, produto) = (soma, produto)
calcular(c:r, soma, produto) = calcular(r, somatorio(c,soma), produtorio(c,produto))

somaProduct(lista) = calcular(lista,0,1)