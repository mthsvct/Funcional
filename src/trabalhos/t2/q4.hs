{- Faça um programa em Haskell que leia uma lista de times de futebol contendo nome do clube, estado e país
a qual pertence e ano de fundação do clube. Faça uma função que ordene a lista pelo campo nome do clube
usando o Quicksort, e depois possibilite ao usuário ver toda a lista e permita também o usuário buscar
informações de um clube pelo nome do clube. (1,5 pontos)
Obs.: cuide para não comparar minúsculas com maiúsculas. -}
module Q4 where

import Data.Char


type Time = (String, String, String, Int)

-- ------------ FUNÇÕES DE LEITURA -------------- --

-- Funções Complementares
times :: [Time]
times = [ ("Flamengo", "Rio de Janeiro", "Brasil", 1895), ("Palmeiras", "São Paulo", "Brasil", 1914), ("Real Madrid", "Madrid", "Espanha", 1902), ("Vasco", "Rio de Janeiro", "Brasil", 1898), ("PSG","Paris","França",1970 ) ]

-- Função que ler string do teclado
lerString = do
    x <- getLine
    return x

-- Função que ler int do teclado
lerInt = do
    putStr "Digite um número: "
    x <- getLine
    return (read x :: Int)


lerLista = do
    putStr "Digite a lista de times: "
    l <- getLine 
    let lista = [ maiscula(x) | x <- (read l :: [Time])]
    return lista


maisc(x) = [ toUpper y | y <- x]

maiscula((nome, estado, pais, ano)) = (maisc(nome), maisc(estado), maisc(pais), ano)

-- ------------ FUNÇÕES DE ORDENAÇÃO -------------- --


quicksort [] = []
quicksort (c:r) = quicksort [x | x <- r, x < c] ++ [c] ++ quicksort [x | x <- r, x >= c]


-- --------------------------- OPCAO 1 do MENU - IMPRIMIR ------------------------- --

imprimir ([]) = putStrLn " "
imprimir((nome,estado,pais,ano):t) = do
    putStrLn ("\nNome do clube: " ++ nome)
    putStrLn ("Estado do clube: " ++ estado)
    putStrLn ("Pais do clube: " ++ pais )
    putStrLn ("Ano de fundacao: " ++ show ano)
    putStrLn " "

    imprimir(t)

-- --------------------- OPCAO 2 do MENU - IMPRIMIR UM TIME ESPECIFICO -------------- --

fst4 (x, _, _, _) = x

busca(t, nome) = [x | x <- t, fst4(x) == nome]

gestaoLerNome = do
    putStr "Digite o nome do clube que voce quer a informacao: "
    nome <- getLine
    return nome

apresenta([]) = putStrLn "Clube nao encontrado! \n"
apresenta(x) = imprimir(x)

especifico(t) = do
    nome <- gestaoLerNome
    let x = busca(t, maisc(nome))
    apresenta(x)

-- --------------------------- FUNÇÕES DO MENU ------------------------- --

erro = putStrLn "Digite um valor valido!"

encerra = putStrLn "Programa encerrado! \n"

{- executarOpcao :: Int -> [Time] -> [Time] -}
executarOpcao 1 t = imprimir(t)
executarOpcao 2 t = especifico(t)
executarOpcao 0 t = encerra
executarOpcao _ t = erro

-- Função que lê a opção do menu
lerOpcao :: IO Int
lerOpcao = do
    putStr "Digite a opção: "
    opcao <- lerInt
    putStrLn ""
    return opcao

menu = do
    putStrLn "Menu"
    putStrLn "1 - Exibir toda a lista"
    putStrLn "2 - Exibir informação especificas sobre um time"
    putStrLn "0 - Sair"

executarMenu t = do
    menu
    op <- lerOpcao

    executarOpcao op t

    if op /= 0 then 
        executarMenu t
    else 
        return t

exT = executarMenu [maiscula(x) | x <- (quicksort times)]


main :: IO ()
main = do
    -- Primeiro ler a lista de times com a função lerLista
    lista <- lerLista

    -- Depois ordenar a lista com a função quicksort
    let listaOrdenada = quicksort lista

    -- Depois executar o menu
    aux <- executarMenu listaOrdenada

    putStrLn "Fim do programa!"
