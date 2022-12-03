-- Questão 1: f1(x) = x2 + 3x - 5x
potencia (base, exp) | exp > 1 = base * potencia(base, exp-1)
                | otherwise = base

f1(x) = potencia(x, 2) + 3 * x - 5 * x


-- Questão 3: f3(x,y,z) = 3xy + 2xz - 3yz
f3(x, y, z) = 3 * x * z + 2 * x * z - 3 * y * z



main = do
        putStrLn "Hello!"