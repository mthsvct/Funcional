module Q1_t2 where

-- Faça um programa em haskell que leia uma lista de strings e então faça uma função para cada item que:

-- A) Devolva uma lista de tuplas, onde cada tupla deve ser composta por uma string e o número de vogais da string. Exemplo: ["abacate", "banana", "uva"] -> [("abacate", 4), ("banana", 3), ("uva", 2)].

-- Função que devolve 1 se o caractere c for vogal e 0 caso contrário
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

contabiliza(lista) = [ ( string, sum([ehVogal(x) | x <- string] ) ) | string <- lista ]

apresentaA([], count) = do
        putStr "\nTotal de vogais em todas as strings: "
        putStrLn (show count)
apresentaA((c, n):r, count) = do
        putStr "A string "
        putStr c
        putStr " tem "
        putStr (show n)
        putStrLn " vogais."
        apresentaA(r, count+n)

principalA(lista) = do
        let aux = contabiliza(lista)
        putStrLn "\nA lista de tuplas eh: "
        putStrLn (show aux)
        putStrLn " "
        apresentaA(contabiliza(lista), 0)

-- --------------------------------------------------------------------------------
-- B) devolva uma lista string contendo as strings que seu tamanho seja maior do que 5 e que inicie com vogais. Exemplo: ["abacate", "banana", "uva"] -> ["abacate"].
mostraS([], cont) = putStrLn ""
mostraS(c:r, cont) = do
        putStr(show cont)
        putStr " - "
        putStrLn(c)
        mostraS(r, cont+1)

{- 1 para contabilizar a ordem das palavras que entram na condição, apresentando uma string por vezda lista. -}
apresentaB(lista) = do
        putStrLn "As strings que iniciam com vogal e tem mais de 5 caracteres sao: "
        mostraS(lista, 1)

-- length = tamanho: lenght(["Matheus", "Victor", "Outra String"]) == 3. lenght([1,3,5,7,9]) == 5.
-- head   = retorna a cabeça (1º item) da lista
compara(lista) = [x | x <- lista, (ehVogal(head x) == 1) && (length(x) > 5)]

principalB(lista) = apresentaB(compara(lista))

-- --------------------------------------------------------------------------------
-- C) devolva a lista de string substituindo as vogais em cada string por @. Exemplo: ["abacate", "banana", "uva"] -> ["@b@c@t@", "@b@n@n@", "@v@"].

substituiStr :: [String] -> [String]
substituiStr(l) = [ [ if ehVogal(x) == 1 then '@' else x | x<-c ] | c<-l ]

