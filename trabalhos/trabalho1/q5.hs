module Q5_trab1 where

-- QuickSort em Haskell
quickSort(lista)
        | null lista == True = []
        | otherwise = quickSort [y | y <- tail lista, y <= head lista] 
                                ++ [head lista] 
                                ++ quickSort [y | y <- tail lista, y > head lista]

{-
        B - uma função que devolva uma lista contendo a 
                soma entre 
                        os quadrados dos elementos das duas listas
                                que forem maiores do que a soma entre o cubo dos 
                                dois primeiros elementos da lista.
-}

potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

cubo(a:b:lista) = potencia(a, 3) + potencia(b, 3)

quadrado([], c) = []
quadrado(a:lista, c)
        | a > c = potencia(a, 2) : quadrado(lista, c)
        | otherwise = quadrado(lista, c)

somaQuadrados([], []) = 0
somaQuadrados(a:lista1, []) = a
somaQuadrados([], b:lista2) = b
somaQuadrados(a:lista1, b:lista2) = (a + b) + somaQuadrados(lista1, lista2)

soma(a, b) = somaQuadrados( quadrado(a, cubo(a)), quadrado(b, cubo(b)) )

