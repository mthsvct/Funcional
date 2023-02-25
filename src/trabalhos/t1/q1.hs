module Q1_trab1 where

{- Função principal para iniciar a execução do programa: fatoracao(...) -}

primo(x, y)
    | y > div x 2 = x
    | mod x y == 0 = primo(x+1, 2)
    | otherwise = primo(x, y+1)

calcula(1, 1, 1, p) = []
calcula(a, b, c, p)
    | mod a p == 0 && mod b p == 0 && mod c p == 0 = p : calcula(div a p, div b p, div c p, p)
    | mod a p == 0 && mod b p == 0 = p : calcula(div a p, div b p, c, p)
    | mod a p == 0 && mod c p == 0 = p : calcula(div a p, b, div c p, p)
    | mod b p == 0 && mod c p == 0 = p : calcula(a, div b p, div c p, p)
    | mod a p == 0 = p : calcula(div a p, b, c, p)
    | mod b p == 0 = p : calcula(a, div b p, c, p)
    | mod c p == 0 = p : calcula(a, b, div c p, p)
    | otherwise = calcula(a, b, c, primo(p+1, 2))

-- Função que calcula a quantidade de vezes que aparece um número na lista.
-- Função que apresenta os resultados.

contabiliza(cabeca, [], contador) = contador
contabiliza(cabeca, c:r, contador)
    | cabeca == c = contabiliza(cabeca, r, contador+1)
    | otherwise = contador


mostraResultados(c, r, contei) = do
        putStr("O numero ")
        putStr(show(c))
        putStr(" aparece ")
        putStr(show(contabiliza(c, r, 1)))
        putStrLn(" vezes.")
        

apresenta([], contei) = putStrLn "FIM!"
apresenta(c:r, contei)
        | c == contei = apresenta(r, contei)
        | otherwise = do 
                mostraResultados(c, r, contei)
                apresenta(r, c)
     
-- Esta função recebe 3 valores e é calculado o fatoração deles.
fatoracao(a, b, c) = apresenta(calcula(a, b, c, 2), 0)

