module Strings where

import Data.Char


-- 1) Função que contabiliza a quantidade vogais presentes em uma string
ehVogal(c)
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | otherwise = 0   

contaVogais([]) = 0
contaVogais(c:r) = ehVogal(toLower(c)) + contaVogais(r)


-- 2) Função que retira as vogais de uma string e retorna a string sem as vogais
retiraVogais([]) = [] -- Caso chegue na condição de parada, retorno uma string vazia
retiraVogais(c:r)
        | ehVogal(toLower(c)) == 1 = retiraVogais(r) -- Verifico se eh vogal e se for, ignoro
        | otherwise = c : retiraVogais(r) -- Caso seja consonante, adiciono a string

-- 3) Função que dada uma string devolva os digitos de uma string
devolveDigitos([]) = []
devolveDigitos(c:r)
        | isDigit(c) == True = c : devolveDigitos(r)
        | otherwise = devolveDigitos(r)

-- 4) Faça uma função em haskell que dada uma string devolva a string em caixa alta.
maiscula([]) = []
maiscula(c:r) = toUpper(c) : maiscula(r)
