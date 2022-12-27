
-- Função que ler um numero inteiro e insere na lista
lerItem(lista) = do 
        putStr "Digite o valor a ser inserido: "
        item <- getLine
        return (lista ++ [(read item :: Int)]) -- retorna a lista com o item inserido 

-- A) Funções de calculo
somatorio(lista, tamanho, i)
        | i == tamanho = 0
        | otherwise = (lista !! i) + somatorio(lista, tamanho, i+1)


-- B) Função que devolve uma lista com os quadrados dos valores
potencia (base, exp) 
        | exp > 1 = base * potencia(base, exp-1)
        | otherwise = base

produto(lista, tamanho, i)
        | i == tamanho = []
        | otherwise = potencia((lista !! i), 2) : produto(lista, tamanho, i+1)

-- C) Função que devolve o maior item da lista
maior(lista, m, i, tamanho)
        | i == tamanho = m
        | i < tamanho && (lista !! i) > m = maior(lista, (lista !! i), i+1, tamanho)
        | otherwise = maior(lista, m, i+1, tamanho)

-- D) Função que devolve uma lista com os multiplos de 3
mult3(lista, i, tamanho)
        | i == tamanho = []
        | mod (lista !! i) 3 == 0 = (lista !! i) : mult3(lista, i+1, tamanho)
        | otherwise = mult3(lista, i+1, tamanho)

-- E) Função que devolve o produto dos itens que estão nos indices impares
prodImpares(lista, i, tamanho)
        | i == tamanho = 1
        | mod i 2 /= 0 = (lista !! i) * prodImpares(lista, i+1, tamanho)
        | otherwise = 1 * prodImpares(lista, i+1, tamanho)


-- -------------------------------------------------- --

menu = do
        putStrLn "MENU: "
        putStrLn "0 - Sair"
        putStrLn "1 - Ler um item e adicionar na lista"
        putStrLn "2 - Visualizar a lista"
        putStrLn "3 - Somatorio da lista"
        putStrLn "4 - Produto da lista"
        putStrLn "5 - Maior item da lista"
        putStrLn "6 - Multiplos de 3"
        putStrLn "7 - Produto dos itens dos indices impares"
        putStr "Digite a opcao desejada: "
        opcao <- getLine
        return (read opcao :: Int)

direcionaFuncoes(op, lista)
        | op == 0 = return lista
        | op == 1 = do 
                lista <- lerItem(lista)
                opcoes(lista)
        | op == 2 = do
                putStrLn (show lista)
                opcoes(lista)
        | op == 3 = do
                putStr "O somatio da lista eh igual a: "
                putStrLn (show (somatorio(lista, length lista, 0)))
                opcoes(lista)
        | op == 4 = do
                putStr "O produto da lista eh igual a: "
                putStrLn (show (produto(lista, length lista, 0)))
                opcoes(lista)
        | op == 5 = do
                putStr "O maior item da lista eh igual a: "
                putStrLn (show (maior(lista, 0, 0, length lista)))
                opcoes(lista)
        | op == 6 = do
                putStr "Os multiplos de 3 da lista sao: "
                putStrLn (show (mult3(lista, 0, length lista)))
                opcoes(lista)
        | op == 7 = do
                putStr "O produto dos itens dos indices impares eh igual a: "
                putStrLn (show (prodImpares(lista, 0, length lista)))
                opcoes(lista)
        
opcoes(lista) = do
        op <- menu
        putStr "\n"
        direcionaFuncoes(op, lista)

main = do
        putStrLn "Hello World!"