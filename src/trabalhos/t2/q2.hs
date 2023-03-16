module Q2_t2 where


{-Faça um programa em Haskell que leia duas listas de strings e então faça pelo menos uma função para cada 
item que:

A) devolva uma lista ordenada contendo strings das duas listas onde as mesmas deve ter tamanho >= a 4 .-} 
separa :: [String] -> [String]
separa(lista) = [c | c <- lista, length c >= 4]

junta(lista1, lista2) = separa(lista1) ++ separa(lista2)

-- so ordena no final 
quicksortListaStringAlfabetica [] = []
quicksortListaStringAlfabetica (c:r) = quicksortListaStringAlfabetica [a | a <- r, a < c] ++ [c] ++ quicksortListaStringAlfabetica [a | a <- r, a >= c]

principalA(lista1, lista2) = quicksortListaStringAlfabetica(junta(lista1, lista2))  

{- B) devolva a quantidade de strings das duas listas que iniciam com vogais -}
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

iniciam(lista) = [ c | c <- lista, ehVogal(head c) == 1 ]

principalB(lista1, lista2) = (length(iniciam(lista1)) + length(iniciam(lista2)))

{- C) devolva uma lista contendo strings das duas listas que iniciam e terminam com vogal -}

iniciamEterminam(lista) = [c | c <- lista, ehVogal(head c) == 1 && ehVogal(last c) == 1]

principalC(lista1, lista2) = iniciamEterminam(lista1) ++ iniciamEterminam(lista2)