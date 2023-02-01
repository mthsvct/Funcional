module Q2_trab1 where

{- Função principal para iniciar a execução do programa: principal(...) -}

{-Essa função de fórmula mostra que temos os parametros q temos e faz o calculo dos ingressos vendidos e as despesas fixas-}
formula(numS, ing, custFixo, custSecao, vendidos) = ( vendidos * ing ) - (custFixo + ( custSecao * numS ))

{-Essa função é para montar uma lista com os valores que foram calculados e que seram apresentados na função apresenta 
valores -}
pegaValores(numSec, vendidos, lucro) = numSec:vendidos:lucro:[]

{-Essa função compara as seguintes condições:
  - se o numero de seções atinge o limite q determinamos para uma condição de parada, para não estrapolar e evitar loop 
  infinito (condição de erro)
  - chamada da função da formula que calcula um valor e se ele for maior que 0 então é ppositivo e esse valor indica lucro
  retornando os valores resultantes a partir da função pega vaalores  -}
calcula(numSec, precoIng, limitePessoas, custoFixo, custoSecao, vendidos, contador, valor)
    | numSec == 20 = -1:[]
    | valor > 0 = pegaValores( numSec, vendidos, valor)
    | contador == limitePessoas = calcula(numSec+1, precoIng, limitePessoas, custoFixo, custoSecao, vendidos+1, 1, formula(numSec+1, precoIng, custoFixo, custoSecao, vendidos+1))
    | otherwise = calcula(numSec, precoIng, limitePessoas, custoFixo, custoSecao, vendidos+1, contador+1, formula(numSec, precoIng, custoFixo, custoSecao, vendidos+1))

apresentaValores(numS:vendidos:lucro:listaResultados) = do 
    putStr("Lucro = ")
    putStrLn (show(lucro))
    putStr("Ingressos vendidos = ") 
    putStrLn (show(vendidos))
    putStr("Número de sesões = ") 
    putStrLn (show(numS))

apresentaErro = putStrLn("Esta combinação de valores não resulta lucro.")

apresenta(c:lista)
    | c == -1 = apresentaErro
    | otherwise = apresentaValores(c:lista)
    
principal(precoIng, limitePessoas, custoFixo, custoSecao) = apresenta( calcula(1, precoIng, limitePessoas, custoFixo, custoSecao, 1, 1, formula(1, precoIng, custoFixo, custoSecao, 1)) )
