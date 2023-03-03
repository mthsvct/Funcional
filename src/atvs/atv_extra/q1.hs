module Q1 where

dec2bin(dec, indice)
    | dec >= 1 = ((mod dec 2) * (10 ^ indice)) + dec2bin(div dec 2, indice + 1)
    | otherwise = 0


