{- Dado uma lista de strings, faça as seguintes funções em Haskell: -}
module Q1 where

import Data.Char

-- a) Que devolva a maior string da lista.
maiorString([], maior) = maior
maiorString(c:r, maior)
        | length c > length maior = maiorString(r, c)
        | otherwise = maiorString(r, maior)

-- b) Que devolva uma lista onde todas as strings são menores do que 6.
menor6([]) = []
menor6(c:r)
        | (length c < 6) = c : menor6(r)
        | otherwise = menor6(r)

-- c) Que conte quantas strings que iniciam com letra maiúscula.
ehMaiscula(c:r)
        | isUpper(c) == True = 1
        | otherwise = 0

qntIniciamMaiusculas([]) = 0
qntIniciamMaiusculas(c:r) = ehMaiscula(c) + qntIniciamMaiusculas(r)

-- d) Que devolva uma lista contendo somente as strings que inicial com a letra maiúscula.
iniciamMaiusculas([]) = []
iniciamMaiusculas(c:r)
        | ehMaiscula(c) == 1 = c : iniciamMaiusculas(r)
        | otherwise = iniciamMaiusculas(r)
