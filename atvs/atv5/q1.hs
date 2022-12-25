
-- Função que ler um numero inteiro e insere na lista
lerItem(lista) = do 
        putStr "Digite o valor a ser inserido: "
        item <- getLine
        return (lista ++ [(read item :: Int)]) -- retorna a lista com o item inserido 

-- Funções de calculo
somatorio(lista, tamanho, i)
        | i == tamanho = 0
        | otherwise = (lista !! i) + somatorio(lista, tamanho, i+1)

menu = do
        putStrLn "MENU: "
        putStrLn "0 - Sair"
        putStrLn "1 - Ler um item e adicionar na lista"
        putStrLn "2 - Visualizar a lista"
        putStrLn "3 - Somatorio da lista"

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
        

opcoes(lista) = do
        op <- menu
        direcionaFuncoes(op, lista)


main = do
        putStrLn "Hello World!"