
buscaCar(lista, c) = sum([ sum([ 1 | caractere <- string, caractere == c ]) | string <- lista ])

maximo l1 l2 = maximum( [maximum(l1), maximum(l2)] )

