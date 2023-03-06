import Data.Char

insereOrdenado(e, []) = [e]
insereOrdenado(e, c:r)
    | e == c = c:r
    | e < c = e:c:r
    | otherwise = insereOrdenado(e, r)

caracteresMaiusculos([], aux) = aux
caracteresMaiusculos(c:r, aux)
    | isUpper( c ) = caracteresMaiusculos(r, insereOrdenado(c, aux))
    | otherwise = caracteresMaiusculos(r, aux)

-- Função quicksort para string

quicksortString [] = []
quicksortString (x:xs) = quicksortString [a | a <- xs, a < x] ++ [x] ++ quicksortString [a | a <- xs, a >= x]

-- Função quicksort para lista de strings

quicksortListaString [] = []
quicksortListaString (x:xs) = quicksortListaString [a | a <- xs, length(a) < length(x)] ++ [x] ++ quicksortListaString [a | a <- xs, length(a) >= length(x)]

-- Função quicksort para lista de strings que deixa em ordem alfabetica

quicksortListaStringAlfabetica [] = []
quicksortListaStringAlfabetica (x:xs) = quicksortListaStringAlfabetica [a | a <- xs, a < x] ++ [x] ++ quicksortListaStringAlfabetica [a | a <- xs, a >= x]