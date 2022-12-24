potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

formula(x, y) = potencia(x, 2) + potencia(y, 3)

somatorio(y, x, n)
        | y < n = formula(x, y) + somatorio(y+1, x, n)
        | y == n = 0

produto(x, n, m)
        | x < m = somatorio(1, x, n) * produto(x+1, n, m) 
        | x == m = 1

produtorio(n, m) = produto(2, n, m)

main = do
        putStrLn "Hello World!"