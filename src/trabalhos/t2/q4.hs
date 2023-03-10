
{- Entrada eh uma string com o nome do time, o estado, o país e um inteiro 
 com o ano da fundacao do clube -}

type Time = (String, String, String, Int)

-- lista de times PARA TESTES
times :: [Time]
times = [ ("Flamengo", "Rio de Janeiro", "Brasil", 1895), ("Palmeiras", "São Paulo", "Brasil", 1914), ("Real Madrid", "Madrid", "Espanha", 1902), ("Vasco", "Rio de Janeiro", "Brasil", 1898), ("PSG","Paris","França",1970 ) ]

-- --------------------------- OPCAO 1 do MENU - IMPRIMIR ------------------------- --

imprimir ([]) = putStrLn " "
imprimir((nome,estado,pais,ano):t) = do
    putStrLn ("Nome do clube: " ++ nome)
    putStrLn ("Estado do clube: " ++ estado)
    putStrLn ("Pais do clube: " ++ pais )
    putStrLn ("Ano de fundacao: " ++ show ano)

    imprimir(t)

-- --------------------- OPCAO 2 do MENU - IMPRIMIR UM TIME ESPECIFICO -------------- --

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

busca(t, nome) = [x | x <- t, fst4(x) == nome]

gestaoLerNome = do
    putStrLn "Digite o nome do clube que voce quer a informacao: "
    nome <- getLine
    return nome

especifico(t) = do
    nome <- gestaoLerNome
    let x = busca(t, nome)
    imprimir(x)
    return t

-- ------------ OPCAO 3 do MENU - LEITURA DE UM NOVO TIME -------------- --

{- Funcao para ler o time -}
lerTime = do
        putStrLn "Digite o nome do clube: "
        nome <- getLine
        putStrLn "Digite o estado do clube: "
        estado <- getLine
        putStrLn "Digite o país do clube: "
        pais <- getLine
        putStrLn "Digite o ano de fundacao do clube: "
        ano <- getLine
        return(nome,estado,pais, read ano :: Int)


{- Gestao para ler um time e insere o time lido dentro da lista -}

gestaoLerTime(t) = do
    x <- lerTime
    return x

-- --------------------------- FUNÇÕES DO MENU ------------------------- --

erro = putStrLn "KRIE VERGONHA NA CARA, digite um valor valido!"


{- executarOpcao :: Int -> [Time] -> [Time] -}
executarOpcao 1 t = do
    imprimir(t)
    return t
executarOpcao 2 t = do
    especifico(t)
    return t
executarOpcao 3 t = do
    x <- gestaoLerTime(t)
    return (x:t)
executarOpcao _ t = do
    erro
    return t


-- Função que lê a opção do menu
lerOpcao = do
    putStr "Digite a opção: "
    opcao <- getLine
    putStrLn ""
    return (read opcao)


menu = do
    putStrLn "Menu"
    putStrLn "1 - Exibir toda a lista"
    putStrLn "2 - Exibir informação especificas sobre um time"
    putStrLn "3 - Ler um novo time"
    putStrLn "0 - Sair"


-- Função que executa o menu
{- executarMenu :: ListaTimes -- AJEITAR -}
executarMenu(t) = do
    menu
    op <- lerOpcao

    if op /= 0 then 
        executarMenu( executarOpcao(op,t) ) 
    else 
        return ()

-- Quando for executar: executarMenu([])


-- Função de ordenação utilizando Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (c:r) = quicksort [x | x <- r, x < c] ++ [c] ++ quicksort [x | x <- r, x >= c]


