media(a, b, c) = (a + b + c) / 3

ehIgual(x, m) 
        | x == m = 1
        | otherwise = 0

calcula(a, b, c) = do
            let m = media(a, b, c)
            let total = ehIgual(a, m) + ehIgual(b, m) + ehIgual(c, m)
            putStrLn (show total)

main :: IO ()
main = do
        putStrLn "Hello!"