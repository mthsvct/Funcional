{-  
    Faça uma função em Haskell que dado dois 
    números inteiros positivos, m e n, devolva 
    o resultado da seguinte equação:
-}
potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

formula(x, y, soma) = soma + (potencia(x, 2) + potencia(y, 3))

somatorio2(n, x, y, soma)
        | y == n = soma
        | otherwise = somatorio2(n, x, y+1, formula(x, y, soma))

somatorio1(n, m, x, soma) 
        | x == n = soma
        | otherwise = somatorio1(n, m, x+1, soma+somatorio2(n, x, 1, 0))

calcula(n, m) = somatorio1(n, m, 2, 0)

main = do 
        putStrLn "Hello World!"