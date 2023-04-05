import Data.Char

ehVogal :: Char -> Int
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0


principalA(lista) = [x | x<-lista, isUpper(head x) && ehVogal(head x) == 1]

verificaVazioA :: [String] -> Maybe [String]
verificaVazioA(lista)
    | length [x | x <- lista, x == ""] > 0 = Nothing
    | otherwise = Just( principalA(lista) )




principalB(lista) = [x | x <- lista, length(x) > 5 && isUpper(head x)]

verificaVazioB :: [String] -> Maybe [String]
verificaVazioB(lista)
    | length [x | x <- lista, x == ""] > 0 = Nothing
    | otherwise = Just( principalB(lista) )




principalC(lista) = [ [ y | y <- x, ehVogal(y) == 0 ]  | x <- lista]

verificaVazioC :: [String] -> Maybe [String]
verificaVazioC(lista)
    | length [x | x <- lista, x == ""] > 0 = Nothing
    | otherwise = Just( principalC(lista) )


