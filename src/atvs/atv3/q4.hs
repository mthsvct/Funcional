potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

produto(x, prod) = 2*potencia(x, 3) + 4*potencia(x, 2) + x

calcula(n, m, prod)
        | n > m = prod
        | otherwise = calcula(n+1, m, produto(n, prod))

main = do
        putStrLn "Hello World!"