module Q3_trab2 where
{- 
    3 - Faça um programa em Haskell que leia duas listas ordenadas de inteiros e então faça pelo menos uma função para cada item:
-}


-- A) devolva duas listas, a primeira contendo os números das posições pares maiores do que 50 
-- e a segunda os elementos ímpares menores que 200.

maiores50(l) = [ l !! p | p <- [0,2.. (length(l)-1) ], l !! p > 50 ]

junta50(l1, l2) = maiores50(l1) ++ maiores50(l2)

menores200(l) = [ x | x <- l, x < 200 && mod x 2 == 1 ]

junta200(l1, l2) = menores200(l1) ++ menores200(l2)

principalA(l1, l2) = (junta50(l1, l2), junta200(l1, l2))

{-
    B) Devolva o produto dos elementos das duas listas dos múltiplos de 3 > 50 e dos múltiplos de 7 menores do que 200.
-} -- PERGUNTAR PRA ELA.

-- SOMA de todos os produtos

mult3(l) = [ x | x <- l, mod x 3 == 0 && x > 50 ]

juntaM3(l1, l2) = mult3(l1) ++ mult3(l2)

mult7(l) = [ x | x <- l, mod x 7 == 0 && x < 200 ]

juntaM7(l1, l2) = mult7(l1) ++ mult7(l2)

produto(lista) = foldr (*) 1 lista

principalB(l1, l2) = produto( juntaM3(l1, l2) ++ juntaM7(l1, l2) )


{- 
    C) Devolva uma lista ordenada contendo elementos das duas listas que sejam maiores do que 50 e que sejam ímpares múltiplos de 3.
    > 50 && mod c 2 /= 0 && mod c 3 == 0
-}

-- Função quicksort
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


{- 2ª forma de fazer a C: -}

m50(lista) = [ x | x <- lista, x > 50 && mod x 2 == 1 && mod x 3 == 0 ]

principalC(l1, l2) = quicksort(m50(l1) ++ m50(l2))
