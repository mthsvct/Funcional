{-  
    Faça uma função em Haskell que dado um intervalo devolva a 
    soma entre o produto dos múltiplos de 3 e o quadrado da soma
    dos divisores de 100.
-}
potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

mult3(inicio, produto)
        | mod inicio 3 == 0 = produto * inicio
        | otherwise = produto
    
somaDivisores(inicio, divisores)
        | mod 100 inicio == 0 = divisores + inicio
        | otherwise = divisores

calcula(inicio, fim, produto, divisores)
        | inicio > fim = produto + potencia(divisores, 2)
        | inicio <= fim = calcula(inicio+1, fim, mult3(inicio, produto), somaDivisores(inicio, divisores))


main = do
    putStrLn "Hello World!"