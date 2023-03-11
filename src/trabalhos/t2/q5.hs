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
type Aluno = (Matricula, Nome, Codigo, Periodo)
type Disciplina = (Codigo, Codigo, Nome, Periodo)
type Notas = (Matricula, Codigo, Nota, Nota)

type Dados = ([Curso], [Aluno], [Disciplina], [Notas])

cursos_ex :: [Curso]
cursos_ex = [ (1, "Ciência da Computação", 10),(2, "Engenharia de Software", 8),(3, "Engenharia de Computação", 5),(4, "Engenharia de Telecomunicações", 10),(5, "Engenharia de Controle e Automação", 10),(6, "Engenharia de Produção", 10),(7, "Engenharia de Materiais", 12),(8, "Engenharia de Mecatrônica", 10),(9, "Engenharia de Energia", 10),(10, "Engenharia de Petróleo", 10), (11, "Enfermagem", 12) ]

-- Faça uma lista de 110 alunos com matrícula, nome, código do curso e período. Onde há 10 alunos por curso, a cada 10 alunos modifica o código (terceiro elemento).
alunos_ex :: [Aluno]
alunos_ex = [(1, "Matheus Victor", 1, 2), (2, "João Pedro", 1, 2), (3, "Pedro Henrique", 1, 2), (4, "Gabriel Henrique", 1, 2), (5, "Arthur Miguel", 1, 2), (6, "Bernardo", 1, 2), (7, "Heitor", 1, 2), (8, "Davi Lucas", 1, 2), (9, "Lorenzo", 1, 2), (10, "Enzo Gabriel", 1, 2), (11, "Murilo", 2, 2), (12, "Luiz Felipe", 2, 2), (13, "Benjamin", 2, 2), (14, "Guilherme", 2, 2), (15, "Samuel", 2, 2), (16, "Nicolas", 2, 2), (17, "Daniel", 2, 2), (18, "Matheus", 2, 2), (19, "Henrique", 2, 2), (20, "Gustavo", 2, 2), (21, "Felipe", 3, 2), (22, "Lucas", 3, 2), (23, "Theo", 3, 2), (24, "Lorenzo", 3, 2), (25, "Benjamin", 3, 2), (26, "Davi", 3, 2), (27, "João", 3, 2), (28, "Guilherme", 3, 2), (29, "Murilo", 3, 2), (30, "Heitor", 3, 2), (31, "Arthur", 4, 2), (32, "Pedro", 4, 2), (33, "Gabriel", 4, 2), (34, "Enzo", 4, 2), (35, "Matheus", 4, 2), (36, "Lorenzo", 4, 2), (37, "Davi", 4, 2), (38, "Benjamin", 4, 2), (39, "João", 4, 2), (40, "Guilherme", 4, 2), (41, "Murilo", 5, 2), (42, "Luiz Felipe", 5, 2), (43, "Benjamin", 5, 2), (44, "Guilherme", 5, 2), (45, "Samuel", 5, 2), (46, "Nicolas", 5, 2), (47, "Daniel", 5, 2), (48, "Matheus", 5, 2), (49, "Henrique", 5, 2), (50, "Gustavo", 5, 2), (51, "Felipe", 6, 2), (52, "Lucas", 6, 2), (53, "Theo", 6, 2), (54, "Lorenzo", 6, 2), (55, "Benjamin", 6, 2), (56, "Davi", 6, 2), (57, "João", 6, 2), (58, "Guilherme", 6, 2), (59, "Murilo", 6, 2), (60, "Heitor", 6, 2), (61, "Arthur", 7, 2), (62, "Pedro", 7, 2), (63, "Gabriel", 7, 2), (64, "Enzo", 7, 2), (65, "Matheus", 7, 2), (66, "Lorenzo", 7, 2), (67, "Davi", 7, 2), (68, "Benjamin", 7, 2), (69, "João", 7, 2), (70, "Guilherme", 7, 2), (71, "Murilo", 8, 2), (72, "Luiz Felipe", 8, 2), (73, "Benjamin", 8, 2), (74, "Guilherme", 8, 2), (75, "Samuel", 8, 2), (76, "Luiza", 9, 6)]


fst3(x, _, _) = x
fst4(x, _, _, _) = x

snd3(_, x, _) = x
snd4(_, x, _, _) = x

trd3(_, _, x) = x
trd4(_, _, x, _) = x

fth4(_, _, _, x) = x


-- ----------------------------- CURSOS ----------------------------- --

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
    putStrLn ("Código do curso:................. " ++ show codigo)
    putStrLn ("Nome do curso:................... " ++ nome)
    putStrLn ("Quantidade de períodos:.......... " ++ show qntPeriodo)
    putStrLn " "
    apresentaCursos t

gestaoApresentaCursos :: [Curso] -> IO [Curso]
gestaoApresentaCursos lista = do
    apresentaCursos lista
    return lista

buscaCurso :: ([Curso], Codigo) -> [Curso]
buscaCurso(cursos, codigo) = [x | x <- cursos, fst3(x) == codigo]

buscaShowCurso(cursos, codigo) = do
    let aux = buscaCurso(cursos, codigo)
    if (aux /= []) 
        then apresentaCursos aux
    else putStrLn "Curso não cadastrado"

-- ----------------------------- ALUNOS ----------------------------- --
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
    return (read matricula, nome, read codigo, read periodo)


getCodCursoAluno((_, _, x, _)) = x

gestaoLerAluno :: [Aluno] -> [Curso] -> IO [Aluno]
gestaoLerAluno alunos cursos = do
    aluno <- lerAluno
    let cod = getCodCursoAluno(aluno)
    let aux = buscaCurso(cursos, cod)
    if (aux == [])
        then do
            putStrLn "\nCurso não cadastrado\nPor favor, digite um codigo valido!\n"
            gestaoLerAluno alunos cursos
    else return (aluno:alunos)
            
apresentaAlunos :: [Aluno] -> [Curso] -> IO ()
apresentaAlunos [] cursos = putStrLn " "
apresentaAlunos ((matricula, nome, codigo, periodo):t) cursos = do
    putStrLn ("Matrícula do aluno:.............. " ++ show matricula)
    putStrLn ("Nome do aluno:................... " ++ nome)
    putStrLn ("Período do aluno:................ " ++ show periodo)
    buscaShowCurso(cursos, codigo)
    putStrLn " "
    apresentaAlunos t cursos

gestaoApresentaAlunos :: [Aluno] -> [Curso] -> IO [Aluno]
gestaoApresentaAlunos lista cursos = do
    if (lista == [])
        then putStrLn "Não há alunos cadastrados"
    else do 
        putStrLn ("------------- ALUNOS CADASTRADOS ---------------")
        apresentaAlunos lista cursos
    return lista

-- ----------------------------- DISCIPLINAS -----------------------------

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


-- ----------------------------- NOTAS ----------------------------- --

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


-- ----------------------------- MENU ----------------------------- --

-- Função menu que mostra as opções do programa

menu :: IO ()
menu = do
    putStrLn "\nMenu"
    putStrLn "1 - Ler Curso"
    putStrLn "2 - Apresentar Cursos"
    putStrLn "3 - Ler Aluno"
    putStrLn "4 - Apresentar Alunos"
    putStrLn "5 - Ler Disciplina"
    putStrLn "6 - Ler Notas"
    putStrLn "0 - Sair"

-- Função que lê a opção do menu
lerOpcao :: IO Int
lerOpcao = do
    putStr "Digite a opção: "
    opcao <- getLine
    putStrLn ""
    return (read opcao)

-- Função que executa a opção do menu

executarCurso :: (Int, [Curso]) -> IO [Curso]
executarCurso(1, lista) = gestaoLerCurso lista
executarCurso(2, lista) = gestaoApresentaCursos lista

executarAluno :: (Int, [Aluno], [Curso]) -> IO [Aluno]
executarAluno(3, alunos, cursos) = gestaoLerAluno alunos cursos
executarAluno(4, alunos, cursos) = gestaoApresentaAlunos alunos cursos


-- Função que executa o menu
executarMenu :: [Curso] -> [Aluno] -> IO ()
executarMenu cursos alunos = do
    menu
    opcao <- lerOpcao
    if opcao == 1 || opcao == 2 then do
        aux <- executarCurso(opcao, cursos)
        executarMenu aux alunos
    else if opcao == 3 || opcao == 4 then do
        aux2 <- executarAluno(opcao, alunos, cursos)
        executarMenu cursos aux2
    else return ()

-- Função principal

main :: IO ()
main = do
    executarMenu [] []
