{- 
    Faça uma função em Haskell com pendência que dado
    dois números inteiros positivos devolva o produto
    entre os números, para isso use somente a operação de soma.
-}

produto(i, f)
        | f > 0 = i + produto(i,f-1)
        | otherwise = 0

main = do
        putStrLn "Hello World!"
