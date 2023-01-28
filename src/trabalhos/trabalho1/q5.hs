module Q5_trab1 where

{- 
    O que falta do trabalho:
        1. Função para ler uma lista de string 4Q
        2. [OK] - Função que contabiliza de vezes que um determinado elemento se repete na lista resultante da Q1.
        3. Função que realiza a junção das listas de União ordenada
-}


{- ------------------------------------------------ -}
{- LETRA A: -}
verificar(e, []) = False
verificar(e,c:l2)
    | e == c = True
    | otherwise = verificar(e,l2)

uniao([],l2) = []
uniao(c:l1, l2)
    | verificar(c, l2) == False = c:uniao(l1,l2)
    | otherwise = uniao(l1,l2)

junta([],[]) = []
junta([], c:l2) = c:junta([],l2)
junta(c:l1,[]) = c:junta(l1,[])
junta(c:l1, c2:l2)
    | c < c2 = c:junta(l1, c2:l2)
    | c >= c2 = c2:junta(c:l1, l2)
    | otherwise = junta(c:l1, c2:l2)

principalA(l1, l2) = junta(uniao(l1, l2), uniao(l2, l1))

{- ------------------------------------------------ -}
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

principal([], []) = [] -- Acrescentei casos de erro
principal([], lista2) = []
principal(lista1, []) = [] 
principal(a:lista1, b:lista2) = somalistas(lista1, lista2, (a^3+b^3))


