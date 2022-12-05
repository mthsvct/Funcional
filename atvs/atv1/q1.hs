ehTriangulo(a, b, c) | (a + b > c) && (b + c > a) && (a + c > b) = True
                | otherwise = False

calcula(a, b, c) = do
        let p = (a + b + c)
        let s = p / 2
        let area = sqrt(s * (s - a) * (s - b) * (s - c))
        putStr "Area: "
        putStrLn(show(area))

area(a,b,c) | ehTriangulo(a, b, c) = calcula(a, b, c)
        | otherwise = putStrLn "Nao eh triangulo"

main = do
        {- 
            area(2, 3, 4) = Eh triangulo e retorna um valor :)
            area(1, 2, 3) = Nao eh triangulo e nao retorna nada :(
        -}
        putStrLn "Hello!"

