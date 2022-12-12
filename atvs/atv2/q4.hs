{- 
    Faça uma função em Haskell que imprima o imposto devido 
    e a restituição de uma pessoa. Considere que
    seja informado o salário bruto mensal e o imposto de renda 
    pago mensal. Os cálculos para obter o imposto
    devido e a restituição são:

    SalarioBrutoAnual = SalarioBrutoMensal * 12
    ImpostoPagoAnual = ImpostoPagoMensal * 12
    BasedeCalculo = SalarioBrutoAnual - 20%
    ImpostoDevido = BaseDeCalculo * Aliquota
    Restituição = ImpostoPago - ImpostoDevido
-}

aliquota(base) 
            | base <= 30000 = 0.0
            | base > 30000 && base <= 40000 = base * 0.075
            | base > 40000 && base <= 50000 = base * 0.15
            | base > 50000 && base <= 60000 = base * 0.225
            | otherwise = base * 0.275


calcula(salarioBrutoMensal, impostoPagoMensal) = do
        let salarioBrutoAnual = salarioBrutoMensal * 12
        let impostoPagoAnual = impostoPagoMensal * 12
        let baseDeCalculo = salarioBrutoAnual - (salarioBrutoAnual * 0.2)
        let impostoDevido = baseDeCalculo * aliquota(baseDeCalculo)
        let restituicao = impostoPagoAnual - impostoDevido
        putStrLn ("\nImposto devido: " ++ show impostoDevido)
        putStrLn ("Restituicao: " ++ show restituicao)

main = do
        putStrLn "Hello World!"