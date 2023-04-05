import Data.Char

ehvogal(c)
    | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
    | otherwise = False

comparar(c)
    | ehvogal(c) == False && c /= 'b' && c /= 'z' = True
    | otherwise = False 

listaBandZ([]) = []
listaBandZ((c:r):lista)
    | comparar(toLower(c)) = (toLower(c):r):listaBandZ(lista)
    | otherwise = listaBandZ(lista)
