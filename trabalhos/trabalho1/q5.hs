-- QuickSort em Haskell
-- Função que recebe uma lista e retorna a lista ordenada

-- Função que recebe uma lista e retorna a lista ordenada
{- quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x] -}

-- Função simplificada que recebe uma lista e retorna a lista ordenada
quickSort(lista)
        | null lista == True = []
        | otherwise = quickSort [y | y <- tail lista, y <= head lista] 
                                ++ [head lista] 
                                ++ quickSort [y | y <- tail lista, y > head lista]




main = do
        putStrLn "Hello World"
