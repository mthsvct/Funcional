module Main where

import Data.Char

data Dia = Segunda|Terca|Quarta|Quinta|Sexta|Sabado|Domingo
    deriving (Eq,Ord,Enum,Show,Read)

converteDias :: [String]
converteDias = [show d | d <- [Segunda .. Domingo]]

filtroDia d listaDias = [d | a <- listaDias, a == d]

diadaSemana ::String -> Maybe Dia
diadaSemana dia
    | length( filtroDia (converte dia) converteDias ) == 1 = Just(read(converte dia) :: Dia)
    | otherwise = Nothing

leDiaSemana :: String -> IO Dia
leDiaSemana mensagem = do
    putStr ("\n")
    putStrLn mensagem
    putStr "Digite um dia da semana: "
    d <- getLine
    case diadaSemana d of
        Nothing -> do
            putStrLn "Dia nao eh valido!!"
            leDiaSemana mensagem
        Just dia -> return (dia)

converte :: String -> String
converte (c:r) = toUpper c:[toLower x | x <- r]

main :: IO()
main = do
    d <- leDiaSemana "Dia"
    putStr("Dia da Semana Retornado: ")
    putStrLn(show d)