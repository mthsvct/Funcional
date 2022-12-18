buscaErro(a, b, c) 
        | (a + b < c) = "C"
        | (b + c < a) = "A"
        | (a + c < b) = "B"
        | otherwise = "X"

mensagem(x) 
        | x == "X" = "Eh triangulo"
        | otherwise = "Nao eh triangulo.\nO lado " ++ x ++ " eh maior que a soma dos outros dois lados."


ehTriangulo(a, b, c) = do
        putStrLn(mensagem(buscaErro(a, b, c)))


main = do
    putStrLn "Hello World!"