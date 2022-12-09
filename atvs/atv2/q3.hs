-- Title: Questão 3

{- 
    Letra A: Se o primeiro número for maior do que o segundo 
    devolva a soma entre cudo do primeiro número e o
    quadrado da divisão do primeiro pelo segundo.
-}

potencia (base, exp) | exp > 1 = base * potencia(base, exp-1)
                | otherwise = base

letraA(a, b) = (potencia(a, 3)) + ( potencia( ( a / b ) , 2) )

{- 
    Letra B: Se o segundo for maior do que o primeiro 
    devolva o produto entre o quadrado do segundo e a 
    diferença entre o segundo e o primeiro.
-}

letraB(a, b) = ( potencia(b, 2) ) * ( b - a )

{- 
    Letra C: Caso os números sejam iguais
    devolva a soma entre a raiz quadrado do primeiro 
    e o quadrado da soma dos dois números.
-}

letraC(a, b) = ( sqrt(a) ) + ( potencia( (a+b), 2 ) )

{-  Função principal: -}
calcula(a, b) | a > b = letraA(a, b)
            | b > a = letraB(a, b)
            | a == b = letraC(a, b)
            | otherwise = 0

main :: IO ()
main = do
        putStrLn "Hello!"