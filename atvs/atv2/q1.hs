buscaErro(a, b, c) | (a + b < c) = "C"
                | (b + c < a) = "A"
                | (a + c < b) = "B"
                | otherwise = "X"


buscaLado(x) | x == 1 = "C"
            | x == 2 = "A"
            | x == 3 = "B"
            | otherwise = "X"


apresenta(x) | x == "X" = "Eh triangulo"
            | otherwise = "Nao eh triangulo.\nO lado " ++ x ++ " eh maior que a soma dos outros dois lados."


ehTriangulo(a, b, c) = do
            let x = buscaErro(a, b, c)
            putStrLn(apresenta(x))


main = do
    putStrLn "Hello World!"