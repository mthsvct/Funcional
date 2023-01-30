import Data.Char
import System.IO (stdout, hFlush)


insere(e,[]) = [e]
insere(e,c:r)
 | e < c = (e:c:r)
 | e == c = (c:r)
 | otherwise = c: insere(e,r)
                

numero([]) = True
numero(c:r)
 | isDigit c = numero r
 | otherwise = False

finalizarLista(0) = True
finalizarLista(n) = False


leituraNro(q,n)
 | q == 1 = do
              hFlush stdout
              putStr "Nro: "
              hFlush stdout
              n1 <- getLine
              leituraNro(2,n1)
 |numero(n) == True = return (read n ::Int)
 |otherwise = do 
               hFlush stdout
               putStr "Numero invalido. \n"
               hFlush stdout
               putStr "digite outro valor. \n"
               hFlush stdout
               leituraNro(1,n)


cadastrarLista(q,n,le)
 |q == 1 = do
            hFlush stdout
            putStr "Digite 0 para terminar a lista. \n"
            nro <- leituraNro(1,"0")
            cadastrarLista(2,nro,le)
 |n /= 0 =  do
             cadastrarLista(1,n,insere(n,le))
 |otherwise = return le


menuPrincipal = do 
                  putStrLn "1 - Cadastrar Lista A"
                  putStrLn "2 - Cadastrar Lista B"
                  putStrLn "3 - Mostrar (A - B)"
                  putStrLn "4 - Mostrar (B - A)"
                  putStrLn "5 - Mostrar união (A - B) e (B - A)"
                  putStrLn "6 - Sair"
                  putStr "Opcao desejada: "
                  op <- getLine
                  return (read op :: Int)	
 
 
casos(1,la,lb) = do
                  lsa <- cadastrarLista(1,0,la)
                  return (lsa,lb)
        
casos(2,la,lb) = do
                  lsb <- cadastrarLista(1,0,lb)
                  return (la,lsb)	
        
casos(3,la,lb) = do
                  --mostrarLista(menos(la,lb))
                  return (la,lb)
        
casos(4,la,lb) = do
                  --mostrarLista(menos(lb,la))
                  return (la,lb)
        
casos(5,la,lb) = do
                  --mostrarLista(uniaoOrd(lb,la))
                  return (la,lb)
        
casos(6,la,lb) = return (la,lb)

        
principal(6,la,lb) = putStr "\n"			
principal(_,la,lb) = do 
                      op <- menuPrincipal
                      (lsa,lsb) <- casos(op,la,lb)
                      principal(op,lsa,lsb)

main = principal(0,[],[])
