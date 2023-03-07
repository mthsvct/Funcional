-- FUnção quicksort de números inteiros
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Função que ordena string
quicksortString [] = []
quicksortString (x:xs) = quicksortString [a | a <- xs, a < x] ++ [x] ++ quicksortString [a | a <- xs, a >= x]

-- Função quicksort para lista de strings

quicksortListaString [] = []
quicksortListaString (x:xs) = quicksortListaString [a | a <- xs, length(a) < length(x)] ++ [x] ++ quicksortListaString [a | a <- xs, length(a) >= length(x)]

-- Função quicksort para lista de strings que deixa em ordem alfabetica

quicksortListaStringAlfabetica [] = []
quicksortListaStringAlfabetica (x:xs) = quicksortListaStringAlfabetica [a | a <- xs, a < x] ++ [x] ++ quicksortListaStringAlfabetica [a | a <- xs, a >= x]