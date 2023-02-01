module Q3_trab1 where
import System.Random (randomRIO)

{- Função principal para iniciar a execução do programa: cadeiras(...) -}

diminui :: Int -> Int
diminui(cadeira)
        | cadeira == 0 = 2
        | otherwise = cadeira - 1

buscaCadeira(num, cadeira)
        | num == 0 = cadeira
        | otherwise = buscaCadeira(num-1, diminui(cadeira))

ocupada(p2, p1)
        | p2 == p1 = diminui(p2)
        | otherwise = p2

sobrou(p1, p2)
        | (p1 == 0 && p2 == 1) || (p1 == 1 && p2 == 0) = 2
        | (p1 == 2 && p2 == 0) || (p1 == 0 && p2 == 2) = 1
        | (p1 == 1 && p2 == 2) || (p1 == 2 && p2 == 1) = 0

apresenta(ana, beatriz, carolina) = do
        putStr("Ana sentou na cadeira: ")
        putStrLn(show(ana))
        putStr("Beatriz sentou na cadeira: ")
        putStrLn(show(beatriz))
        putStr("Carolina sentou na cadeira: ")
        putStrLn(show(carolina))

cadeiras :: Int -> IO() 
cadeiras(limite) = do
        a <- randomRIO(1, limite::Int) 
        b <- randomRIO(1, limite::Int) 
        let ana = buscaCadeira(a, 1)
        let beatriz = ocupada(buscaCadeira(b, 1), ana)
        let carolina = sobrou(ana, beatriz)
        apresenta(ana, beatriz, carolina)

main = do 
        putStrLn "Hello World!"