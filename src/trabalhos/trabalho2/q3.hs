module Q3_trab2 where
{- 
    3 - Faça um programa em Haskell que leia duas listas ordenadas de inteiros e então faça pelo menos uma função para cada item:
-}


-- A) devolva duas listas, a primeira contendo os números das posições pares maiores do que 50 e a segunda os elementos ímpares menores que 200.
adicionaFinal([], y) = [y]
adicionaFinal(c:r, y) = c : adicionaFinal(r, y)

maiores50(c, lista, i)
    | mod i 2 == 0 && c > 50 = adicionaFinal(lista, c)
    | otherwise = lista

impares200(c, lista)
    | mod c 2 /= 0 && c < 200 = adicionaFinal(lista, c)
    | otherwise = lista

busca(c1:l1, c2:l2, i1, i2, m50, im200)
    
    
principalA(l1, l2) = busca(l1, l2, 0, 0, [], [])
