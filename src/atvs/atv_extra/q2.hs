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