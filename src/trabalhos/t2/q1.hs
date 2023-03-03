module Q1_trab1 where


-- Faça um programa em haskell que leia uma lista de strings e então faça uma função para cada item que:


-- A) Devolva uma lista de tuplas, onde cada tupla deve ser composta por uma string e o número de vogais da string. Exemplo: ["abacate", "banana", "uva"] -> [("abacate", 4), ("banana", 3), ("uva", 2)].

-- Função que devolve 1 se o caractere c for vogal e 0 caso contrário
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

-- [1, 0, 1, 0, 1, 0, 1]

soma([]) = 0
soma(c:r) = c + soma(r)

contabiliza([]) = []
contabiliza(c:r) = (c, soma([ehVogal(x) | x <- c])) : contabiliza(r)

apresentaA([]) = putStrLn ""
apresentaA((c, n):r) = do
        putStr "A string "
        putStr c
        putStr " tem "
        putStr (show n)
        putStrLn " vogais"
        apresentaA(r)

principalA(lista) = apresentaA(contabiliza(lista))

-- --------------------------------------------------------------------------------
-- B) devolva uma lista string contendo as strings que seu tamanho seja maior do que 5 e que inicie com vogais. Exemplo: ["abacate", "banana", "uva"] -> ["abacate"].
mostraS([], cont) = putStrLn ""
mostraS(c:r, cont) = do
        putStr(show cont)
        putStr " - "
        putStrLn(c)
        mostraS(r, cont+1)

{-1 para contabilizar a ordem das palavras que entram na condição, apresentando uma string por vez
da lista -} 

apresentaB(lista) = do
        putStrLn "As strings que iniciam com vogal e tem mais de 5 caracteres sao: "
        mostraS(lista, 1)

-- length = tamanho
-- head = cabeça da lista

compara(lista) = [x | x <- lista, (ehVogal(head x) == 1) && (length(x) > 5)]

principalB(lista) = apresentaB(compara(lista))

-- --------------------------------------------------------------------------------
-- C) devolva a lista de string substituindo as vogais em cada string por @. Exemplo: ["abacate", "banana", "uva"] -> ["@b@c@t@", "@b@n@n@", "@v@"].

{- poeArr([]) = []
poeArr(c:r)
    | ehVogal(c) == 1 = '@':poeArr(r)
    | otherwise = c:poeArr(r) -- c eh um caractere -}

-- PERGUNTAR PARA ELA.
arroba(c)
        | ehVogal(c) == 1 = '@'
        | otherwise = c

substitui([]) = []
substitui(c:r) = [arroba(x) | x <- c] : substitui(r)

apresentaC(lista) = do
        putStrLn "As strings com as vogais substituidas por @ sao: "
        mostraS(lista, 1)

principalC(lista) = apresentaC(substitui(lista))
