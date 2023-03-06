
soma([]) = 0
soma(c:r) = c + soma(r)

somaArq(pasta) = do
    -- Lendo um arquivo com números e somando
    nums <- readFile "num.txt"
    -- Transformando a string em uma lista de inteiros
    let numeros = map read (lines nums) :: [Int]
    -- Somando os números
    let n = soma(numeros)
    return n


main :: IO ()
main = do

    -- Lendo o arquivo
    contents <- readFile "teste.txt"

    -- Escrevendo no arquivo
    writeFile "teste2.txt" "Escrevendo no arquivo\n"

    -- Adicionando no arquivo
    appendFile "teste2.txt" "Adicionando no arquivo\n"

    putStr contents