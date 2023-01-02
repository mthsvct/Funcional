module Q4_trab1 where
import Data.Char


-- a) uma função para contar o número de caracteres que cada string possui sem repetir;
foiLido(caractere, []) = False
foiLido(caractere, c:r)
        | caractere == c = True
        | otherwise = foiLido(caractere, r)

qntSemRepetir([], lidos) = 0
qntSemRepetir(c:r, lidos)
        | foiLido(c, lidos) = qntSemRepetir(r, lidos)
        | otherwise = 1 + qntSemRepetir(r, lidos ++ [c])

-- lista de strings
listaStr([]) = putStrLn " "
listaStr(c:r) = do
        putStrLn c
        listaStr(r)


{- 
        B) uma  função  que  devolve  uma  lista  contendo  os  tipos  de  
        caracteres  que  iniciam  as  strings  da  lista,  por exemplo: 
        vogal, dígito ou outro tipo de carácter e se o mesmo é ou 
        não maiúsculo, quando possível. -}
tipoCar(c)
        | isSpace(c) = "espaço"
        | isPunctuation(c) = "pontuacao"
        | ehVogal(c) == 1 = "vogal"
        | ehVogal(c) == 0 = "consoante"
        | isUpper(c) = "maiuscula"
        | isLower(c) = "minuscula"
        | otherwise = "outro"

tipo(c:r) 
        | isDigit(c) = "digito"
        | otherwise = tipoCar(c)

tipos([]) = []
tipos(string:listaString) = tipo(string) : tipos(listaString)



-- Função que devolve 1 se o caractere c for vogal e 0 caso contrário
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

-- Função que contabiliza a quantidade de vogais de uma string
quantidade([]) = 0
quantidade(c:r) = ehVogal(c) + quantidade(r)

-- Função que devolve a string que possui a maior quantidade de vogais.
maiorQntVogais([], maior, qnt) = maior
maiorQntVogais(c:r, maior, qnt)
        | quantidade(c) > qnt = maiorQntVogais(r, c, quantidade(maior))
        | otherwise = maiorQntVogais(r, maior, qnt)

