media(a, b, c) = (a + b + c) / 3


ehIgual(x, m) 
        | x == m = 1
        | otherwise = 0

compara(a, b, c, m) = ehIgual(a, m) + ehIgual(b, m) + ehIgual(c, m)

calcula(a, b, c) = do
            {- 
            let m = media(a, b, c)
            let total = ehIgual(a, m) + ehIgual(b, m) + ehIgual(c, m) 
            -}
            putStrLn (show compara(a, b, c, media(a, b, c) ))


main :: IO ()
main = do
        putStrLn "Hello!"