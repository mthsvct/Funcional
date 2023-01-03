

custo(x)
    | ((x*15000) - (50000+(x*10000))) > 0 = x
    | otherwise = custo(x+1)


custoteste(numsec, ing, pessoas, custfix,  valext)
    | numsec == 20 = -1
    | ((numsec*(ing*pessoas)) - (custfix+(numsec*valext))) > 0 = numsec
    | otherwise = custoteste(numsec+1,ing,pessoas,custfix,valext) 


lucro(numsec, ing, pessoas, custfix, valext) = ((numsec*(ing*pessoas)) - (custfix+(numsec*valext)))

ingvend(numsec, pessoas) = (numsec*pessoas)

apresenta(lucro, ingvend, numsec) = do 
    putStrLn("Lucro = " ++ show(lucro))
    putStrLn("Ingressos vendidos = " ++ show(ingvend))
    putStrLn("Número de sesões = " ++ show(numsec))

apresentaErro = putStrLn ("Esses valores nao resultam lucro")

principal(ing, pessoas, custfix, valext)
    | custoteste (1, ing, pessoas, custfix,  valext) == -1 = apresentaErro
    | otherwise = apresenta (lucro(custoteste(1, ing, pessoas, custfix,  valext), ing, pessoas, custfix, valext), ingvend (custoteste (1, ing, pessoas, custfix, valext), pessoas), custoteste(1, ing, pessoas, custfix,  valext))

