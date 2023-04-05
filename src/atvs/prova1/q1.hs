verificar(n, i)
    | i > div n 2 = 1
    | mod n i == 0 = 0
    | otherwise = verificar(n, i+1)

primo(n) = verificar(n, 2)