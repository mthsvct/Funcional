-- Letra A: Faça uma função que devolva o valor de x.
buscaValor(x) | ( 2 * x + 5 ) + (x + 10) == 180 = x
        | otherwise = buscaValor(x+1) 

-- Letra B: Faça uma função que devolva o valor dos dois ângulos.
    -- Letra B.1: 2x + 5
    -- Letra B.2: x + 10
anguloA = 2*buscaValor(1) + 5
anguloB = buscaValor(1) + 10

main = do
        putStrLn "Hello!"