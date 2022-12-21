-- Verifica se a lista está vazia
ehVazia x = null x

-- Retorna o tamanho da lista
tamanho x = length x

-- Retorna o primeiro elemento da lista
primeiro x = head x

-- Retorna o último elemento da lista
ultimo x = last x

-- Retorna a lista sem o primeiro elemento
resto x = tail x

-- Retorna a lista sem o último elemento
inicio x = init x

-- Retorna o elemento na posição n
elemento n x = x !! n

-- Retorna a lista reversa
reverso x = reverse x

-- Retorna a lista com o elemento x no final
adiciona x y = x ++ y

-- Retorna a lista com o elemento x no início
adicionaInicio x y = x : y

-- Retorna a lista sem o elemento x
remove x y = filter (/= x) y

-- Retorna a lista sem o elemento na posição n
removePos n x = take n x ++ drop (n+1) x

-- Retorna a lista com o elemento x na posição n
inserePos n x y = take n y ++ x ++ drop n y

-- Retorna a lista com o elemento x substituindo o elemento na posição n
substituiPos n x y = take n y ++ x ++ drop (n+1) y

-- junta duas listas em uma única lista formada pelos pares dos elementos correspondentes
zipa x y = zip x y

-- Soma todos os elementos da lista
soma x = sum x

main = do
        putStrLn "Hello World"