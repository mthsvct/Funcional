-- A função map aplica uma função a todos os elementos de uma lista

ehVogal :: Char -> Int
ehVogal(c) 
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = 1
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = 1
        | otherwise = 0

ehVogalBool :: Char -> Bool
ehVogalBool(c)
        | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
        | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' = True
        | otherwise = False

divs n = [c | c <- [1..n], mod n c == 0]

primo n = divs n == [1,n]

-- EXEMPLOS:

qntVogal string = sum( map (ehVogal) string )

dobra lista = map (*2) lista



-- Função filter filtra uma lista de acordo com uma condição

vogais string = filter (ehVogalBool) string

primos n = filter (primo) [1..n]

mult x y = mod y x == 0
multiplos n lista = filter (mult n) lista

