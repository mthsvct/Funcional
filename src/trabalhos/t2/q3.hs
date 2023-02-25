module Q3_trab2 where
{- 
    3 - Faça um programa em Haskell que leia duas listas ordenadas de inteiros e então faça pelo menos uma função para cada item:
-}


-- A) devolva duas listas, a primeira contendo os números das posições pares maiores do que 50 
-- e a segunda os elementos ímpares menores que 200.


monta50(c, i, rL1)
    | mod i 2 == 0 && c > 50 = c:rL1
    | otherwise = rL1

monta200(c, i, rL1)
    | i `mod` 2 == 1 && c < 200 = c:rL1
    | otherwise = rL1

montaListas([], i, [], j, l50, l200) = (reverse(l50), reverse(l200))
montaListas([], i, c2:l2, j, l50, l200) = montaListas([], i, l2, j+1, monta50(c2, j, l50), monta200(c2, j, l200))
montaListas(c:l1, i, [], j, l50, l200) = montaListas(l1, i+1, [], j, monta50(c, i, l50), monta200(c, i, l200))
montaListas(c:l1, i, c2:l2, j, l50, l200)
    | c < c2 = montaListas(l1, i+1, c2:l2, j, monta50(c, i, l50), monta200(c, i, l200))
    | c > c2 = montaListas(c:l1, i, l2, j+1, monta50(c2, j, l50), monta200(c2, j, l200))
    | otherwise = montaListas(l1, i+1, l2, j+1, monta50(c, i, l50), monta200(c, i, l200))

principalA(l1, l2) = montaListas(l1, 0, l2, 0, [], [])