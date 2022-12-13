import System.Random (randomRIO)

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
        putStrLn ("Ana sentou na cadeira: " ++ show(ana))
        putStrLn ("Beatriz sentou na cadeira: " ++ show(beatriz))
        putStrLn ("Carolina sentou na cadeira: " ++ show(carolina))


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