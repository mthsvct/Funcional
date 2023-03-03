module TypeNiver where

type Nome = String
type Idade = Int
type PessoaIdade = (Idade, Nome)

idP :: PessoaIdade -> Idade
idP (i, _) = i

nomeP :: PessoaIdade -> Nome 
nomeP (_, n) = n 

data Aniversario = Nascimento Int Int Int |
                   Casamento String String Int Int Int 

mensagemDataNas :: Aniversario -> IO()
mensagemDataNas (Nascimento dia mes ano) = putStrLn ("Nasceu em " ++ show dia ++ "/" ++ show mes ++ "/" ++ show ano)
mensagemDataNas (Casamento nome1 nome2 dia mes ano) = putStrLn (nome1 ++ " e " ++ nome2 ++ " casaram em " ++ show dia ++ "/" ++ show mes ++ "/" ++ show ano)

-- Como colocar na função mensagemDataNas:
--      -> mensagemDataNas (Nascimento 22 12 2000)
--      -> mensagemDataNas (Casamento "João" "Maria" 22 12 2000)