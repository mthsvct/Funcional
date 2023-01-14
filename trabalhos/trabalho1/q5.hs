{- LETRA A: -}
insercao(e, []) = [e]
insercao(e, c:r)
    | e < c = e:c:r -- Condiçao de parada, encontrei a posição certa.
    | e == c = c:r
    | otherwise = c:insercao(e, r)


ordena([], ord) = ord
ordena(c:r, ord) = ordena(r, insercao(c, ord))

junta([],[]) = []
junta([],c:l2) = c:junta([],l2)
junta(c:l1,l2) = c:junta(l1,l2)

verificar(e, []) = False
verificar(e,c:l2)
    | e == c = True
    | otherwise = verificar(e,l2)

uniao([],l2) = []
uniao(c:l1, l2)
    | verificar(c, l2) == False = c:uniao(l1,l2)
    | otherwise = uniao(l1,l2)


listas(l1, l2) = ordena(junta(uniao(l1, l2), uniao(l2, l1)), [])

{- LETRA B: -}

somalistas([], [], cubo) = []
somalistas([], b:lista2, cubo)
    | (0^2 + b^2) > cubo = (0^2 + b^2):somalistas([], lista2, cubo)
    | otherwise = somalistas([], lista2, cubo)
somalistas(a:lista1, [], cubo)
    | (a^2 + 0^2) > cubo = (a^2 + 0^2):somalistas(lista1, [], cubo)
    | otherwise = somalistas(lista1, [], cubo)
somalistas (a:lista1, b:lista2, cubo)
    | (a^2+b^2) > cubo = (a^2+b^2):somalistas(lista1, lista2, cubo)
    | otherwise = somalistas(lista1, lista2, cubo)

principal(a:lista1, b:lista2) = somalistas(lista1, lista2, (a^3+b^3))


