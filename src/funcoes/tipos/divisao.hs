module Main where

divisao :: Int -> Int -> Maybe Int
divisao x y
    |  y == 0 = Nothing
    | otherwise = Just(div x y)

main :: IO ()
main = do 
    putStrLn "Digite dois numeros: "
    a <- getLine
    b <- getLine
    case divisao (read a::Int) (read b::Int) of
        Nothing -> do
            putStrLn "Divisao por zero"
            putStrLn "Tente novamente"
            main
        Just z -> putStrLn("Resposta: "++show(z))