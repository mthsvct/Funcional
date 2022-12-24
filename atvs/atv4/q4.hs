potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

formula(x) = (2 * potencia(x, 4)) + (4 * potencia(x, 3)) + (potencia(x, 2))

somatorio(n, m)
        | n <= m = formula(n) + somatorio(n+1, m)
        | otherwise = 0

main = do
        putStrLn "Hello World!"