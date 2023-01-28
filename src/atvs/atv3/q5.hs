-- dividir sem utilizar o operador de divisão e usar apenas subtração.

dividir(a, b, contador)
        | b-a < 0 = contador
        | otherwise = dividir(a, b-a, contador+1)

calcula(a, b)
        | a > b = calcula(b, a)
        | b == 0 = -1
        | otherwise = dividir(a, b, 0)

main = do
        putStrLn "Hello World!"