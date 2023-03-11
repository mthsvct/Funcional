module Q5_t2 where

{- 
    alunos(Matrícula, Nome, Curso, Período), 
    curso(Código, Nome, Quantidade de Períodos), 
    Disciplinas(Código da Disciplina, Código do Curso, Nome Disciplina, Período) e 
    Notas(Matricula, Código Disciplina, Nota1, Nota2).
-}

type Matricula = Int
type Nome = String
type Codigo = Int
type QntPeriodo = Int
type Periodo = Int
type Nota = Float

type Curso = (Codigo, Nome, QntPeriodo)
type Aluno = (Matricula, Nome, Curso, Periodo)
type Disciplina = (Codigo, Codigo, Nome, Periodo)
type Notas = (Matricula, Codigo, Nota, Nota)

type Dados = ([Curso], [Aluno], [Disciplina], [Notas])

-- Função que ler um curso pelo o teclado
lerCurso :: IO Curso
lerCurso = do
    putStr "Digite o código do curso: "
    codigo <- getLine
    putStr "Digite o nome do curso: "
    nome <- getLine
    putStr "Digite a quantidade de períodos do curso: "
    qntPeriodo <- getLine
    return (read codigo, nome, read qntPeriodo)

{- gestaoLerCurso :: [Curso] -> IO [Curso] -}
gestaoLerCurso lista = do
    curso <- lerCurso
    putStrLn ("Curso: " ++ show curso)
    return (curso:lista)


-- Função que apresenta os cursos cadastrados

apresentaCursos :: [Curso] -> IO ()
apresentaCursos [] = putStrLn " "
apresentaCursos ((codigo, nome, qntPeriodo):t) = do
    putStrLn ("Código do curso: " ++ show codigo)
    putStrLn ("Nome do curso: " ++ nome)
    putStrLn ("Quantidade de períodos: " ++ show qntPeriodo)
    putStrLn " "
    apresentaCursos t

gestaoApresentaCursos :: [Curso] -> IO [Curso]
gestaoApresentaCursos lista = do
    apresentaCursos lista
    return lista

-- Função que lê um aluno pelo o teclado
lerAluno :: IO Aluno
lerAluno = do
    putStr "Digite a matrícula do aluno: "
    matricula <- getLine
    putStr "Digite o nome do aluno: "
    nome <- getLine
    putStr "Digite o código do curso do aluno: "
    codigo <- getLine
    putStr "Digite o período do aluno: "
    periodo <- getLine
    return (read matricula, nome, (read codigo, "", 0), read periodo)

gestaoLerAluno :: IO ()
gestaoLerAluno = do
    aluno <- lerAluno
    putStrLn ("Aluno: " ++ show aluno)

-- Função que lê uma disciplina pelo o teclado

lerDisciplina :: IO Disciplina
lerDisciplina = do
    putStr "Digite o código da disciplina: "
    codigo <- getLine
    putStr "Digite o código do curso da disciplina: "
    codigoCurso <- getLine
    putStr "Digite o nome da disciplina: "
    nome <- getLine
    putStr "Digite o período da disciplina: "
    periodo <- getLine
    return (read codigo, read codigoCurso, nome, read periodo)

gestaoLerDisciplina :: IO ()
gestaoLerDisciplina = do
    disciplina <- lerDisciplina
    putStrLn ("Disciplina: " ++ show disciplina)

-- Função que lê as notas de um aluno

lerNotas :: IO Notas
lerNotas = do
    putStr "Digite a matrícula do aluno: "
    matricula <- getLine
    putStr "Digite o código da disciplina: "
    codigo <- getLine
    putStr "Digite a nota 1 do aluno: "
    nota1 <- getLine
    putStr "Digite a nota 2 do aluno: "
    nota2 <- getLine
    return (read matricula, read codigo, read nota1, read nota2)

gestaoLerNotas :: IO ()
gestaoLerNotas = do
    notas <- lerNotas
    putStrLn ("Notas: " ++ show notas)


-- Função menu que mostra as opções do programa

menu :: IO ()
menu = do
    putStrLn "\nMenu"
    putStrLn "1 - Ler Curso"
    putStrLn "2 - Apresentar Cursos"
    putStrLn "3 - Ler Aluno"
    putStrLn "4 - Ler Disciplina"
    putStrLn "5 - Ler Notas"
    putStrLn "0 - Sair"

-- Função que lê a opção do menu
lerOpcao :: IO Int
lerOpcao = do
    putStr "Digite a opção: "
    opcao <- getLine
    putStrLn ""
    return (read opcao)

-- Função que executa a opção do menu

executarOpcao :: (Int, [Curso]) -> IO [Curso]
executarOpcao(1, lista) = gestaoLerCurso lista
executarOpcao(2, lista) = gestaoApresentaCursos lista
{- executarOpcao(2, lista) = gestaoLerAluno
executarOpcao(3, lista) = gestaoLerDisciplina
executarOpcao(4, lista) = gestaoLerNotas 
executarOpcao(0, lista) = lista -}


-- Função que executa o menu
executarMenu :: [Curso] -> IO ()
executarMenu cursos = do
    menu
    opcao <- lerOpcao
    aux <- executarOpcao(opcao, cursos)
    if opcao /= 5 then executarMenu aux else return ()

-- Função principal

main :: IO ()
main = do
    executarMenu []
