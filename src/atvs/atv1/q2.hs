-- Letra A: f1(x) = x2 + 3x - 5x
potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

f1(x) = potencia(x, 2) + 3 * x - 5 * x

-- --------------------------------------
-- Letra B: f2(x,y) = 2x3 - 3xy  + 10y
f2(x, y) = 2*potencia(x, 3) - 3*x*y + 10*y

-- --------------------------------------
-- Letra C: f3(x,y,z) = 3xy + 2xz - 3yz
f3(x, y, z) = ((3 * x) * y) + ((2 * x) * z) - ((3 * y) * z)

-- --------------------------------------
-- Letra D: f4(x,y,z) = 4x 2 y 3 - 2y 2 z + 3yz 2
f4(x, y, z) = 4*potencia(x, 2)*potencia(y, 3) - 2*potencia(y, 2)*z + 3*y*potencia(z, 2)

-- --------------------------------------
-- Letra E: f(x,y) = (x + y)2/ 2(x + y)
f5(x, y) = potencia( (x+y), 2 ) / (2*(x+y))

main = do
        putStrLn "Hello!"