module Q1_trab1 where


-- Faça um programa em haskell que leia uma lista de strings e então faça uma função para cada item que:


-- A) Devolva uma lista de tuplas, onde cada tupla deve ser composta por uma string e o número de vogais da string. Exemplo: ["abacate", "banana", "uva"] -> [("abacate", 3), ("banana", 3), ("uva", 1)].

-- Função que devolve 1 se o caractere c for vogal e 0 caso contrário
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

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