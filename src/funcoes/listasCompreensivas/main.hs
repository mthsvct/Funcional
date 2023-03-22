listaPares(le) = [a | a <- le, even(a)]

listaParesM20(le) = [a | a <- le, even(a), a>20]

listaParesMult3(le) = [a | a <- le, mod a 3 == 0]

listaParesMult3ou5(le) = [a | a <- le, (mod a 3 == 0 || mod a 5 == 0)]

dobroLista(le) = [a * 2 | a <- le]

produtoCarteziano(l1, l2) = [(a,b) | a <- l1, b <- l2]

somaQuadrados(l1, l2) = [(a+b)^2 | a <- l1, b<-l2]

divs n = [c | c <- [1..n], mod n c == 0]

primo n = divs n == [1,n]

-- Se for primo retorne 1 sem usar pipe
divisores n = length [ c | c <- [1.. (div n 2)], mod n c == 0]

primo n = if divisores n == 1 then 1 else 0