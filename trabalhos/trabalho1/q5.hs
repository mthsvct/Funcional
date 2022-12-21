-- QuickSort em Haskell
quickSort(lista)
        | null lista == True = []
        | otherwise = quickSort [y | y <- tail lista, y <= head lista] 
                                ++ [head lista] 
                                ++ quickSort [y | y <- tail lista, y > head lista]




main = do
        putStrLn "Hello World"
