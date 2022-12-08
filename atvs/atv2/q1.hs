buscaErro(a, b, c) | (a + b < c) = 1
                | (b + c < a) = 2
                | (a + c < b) = 3
                | otherwise = 0


buscaLado(x) | x == 1 = "C"
            | x == 2 = "A"
            | x == 3 = "B"
            | otherwise = "X"


ehTriangulo(a, b, c) = do
                let x = buscaErro(a, b, c)
                if x == 0
                    then putStrLn "Eh triangulo"
                    else do
                        let lado = buscaLado(x)
                        putStrLn("Nao eh triangulo. O lado " ++ lado ++ " eh maior que a soma dos outros dois lados. ")


main = do
    putStrLn "Hello World!"