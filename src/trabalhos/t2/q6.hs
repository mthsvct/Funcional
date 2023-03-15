module Q6_t2 where

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

arqAlunos = "data/alunos.txt"
arqCursos = "data/cursos.txt"
arqDisciplinas = "data/disciplinas.txt"
arqNotas = "data/notas.txt"

abrirAlunos :: IO [Aluno]
abrirAlunos = do
    conteudo <- readFile "data/alunos.txt"
    let alunos = map read (lines conteudo) :: [Aluno]
    return alunos

addAluno((matricula, nome, codigo, periodo))= do
    let novo = "(" ++ show matricula ++ ", \"" ++ nome ++ "\" , " ++ show codigo ++ ", " ++ show periodo ++ ")\n"
    appendFile arqAlunos novo

-- ------------------------------ CURSOS ------------------------------ --

abrirCursos :: IO [Curso]
abrirCursos = do
    conteudo <- readFile "data/cursos.txt"
    let cursos = map read (lines conteudo) :: [Curso]
    return cursos

addCurso((codigo, nome, qntPeriodo))= do
    let novo = "(" ++ show codigo ++ ", \"" ++ nome ++ "\", " ++ show qntPeriodo ++ ")\n"
    appendFile arqCursos novo

-- ------------------------------ DISCIPLINAS ------------------------------ --

abrirDisciplinas :: IO [Disciplina]
abrirDisciplinas = do
    conteudo <- readFile "data/disciplinas.txt"
    let disciplinas = map read (lines conteudo) :: [Disciplina]
    return disciplinas

addDisciplina((codigo, codigoCurso, nome, periodo))= do
    let novo = "(" ++ show codigo ++ ", " ++ show codigoCurso ++ ", \"" ++ nome ++ "\", " ++ show periodo ++ ")\n"
    appendFile arqDisciplinas novo

-- ------------------------------ NOTAS ------------------------------ --

abrirNotas :: IO [Notas]
abrirNotas = do
    conteudo <- readFile "data/notas.txt"
    let notas = map read (lines conteudo) :: [Notas]
    return notas

addNotas((matricula, codigo, nota1, nota2))= do
    let novo = "(" ++ show matricula ++ ", " ++ show codigo ++ ", " ++ show nota1 ++ ", " ++ show nota2 ++ ")\n"
    appendFile arqNotas novo


-- --------------------------- FUNÇÕES PRONTAS -------------------------- --

cursos_ex :: [Curso]
cursos_ex = [ (1, "Ciência da Computação", 10),(2, "Engenharia de Software", 8),(3, "Engenharia de Computação", 5),(4, "Engenharia de Telecomunicações", 10),(5, "Engenharia de Controle e Automação", 10),(6, "Engenharia de Produção", 10),(7, "Engenharia de Materiais", 12),(8, "Engenharia de Mecatrônica", 10),(9, "Engenharia de Energia", 10),(10, "Engenharia de Petróleo", 10), (11, "Enfermagem", 12) ]

-- Faça uma lista de 110 alunos com matrícula, nome, código do curso e período. Onde há 10 alunos por curso, a cada 10 alunos modifica o código (terceiro elemento).
alunos_ex :: [Aluno]
alunos_ex = [(1, "Matheus Victor", 1, 2), (2, "João Pedro", 1, 2), (3, "Pedro Henrique", 1, 2), (4, "Gabriel Henrique", 1, 2), (5, "Arthur Miguel", 1, 2), (6, "Bernardo", 1, 2), (7, "Heitor", 1, 2), (8, "Davi Lucas", 1, 2), (9, "Lorenzo", 1, 2), (10, "Enzo Gabriel", 1, 2), (11, "Murilo", 2, 2), (12, "Luiz Felipe", 2, 2), (13, "Benjamin", 2, 2), (14, "Guilherme", 2, 2), (15, "Samuel", 2, 2), (16, "Nicolas", 2, 2), (17, "Daniel", 2, 2), (18, "Matheus", 2, 2), (19, "Henrique", 2, 2), (20, "Gustavo", 2, 2), (21, "Felipe", 3, 2), (22, "Lucas", 3, 2), (23, "Theo", 3, 2), (24, "Lorenzo", 3, 2), (25, "Benjamin", 3, 2), (26, "Davi", 3, 2), (27, "João", 3, 2), (28, "Guilherme", 3, 2), (29, "Murilo", 3, 2), (30, "Heitor", 3, 2), (31, "Arthur", 4, 2), (32, "Pedro", 4, 2), (33, "Gabriel", 4, 2), (34, "Enzo", 4, 2), (35, "Matheus", 4, 2), (36, "Lorenzo", 4, 2), (37, "Davi", 4, 2), (38, "Benjamin", 4, 2), (39, "João", 4, 2), (40, "Guilherme", 4, 2), (41, "Murilo", 5, 2), (42, "Luiz Felipe", 5, 2), (43, "Benjamin", 5, 2), (44, "Guilherme", 5, 2), (45, "Samuel", 5, 2), (46, "Nicolas", 5, 2), (47, "Daniel", 5, 2), (48, "Matheus", 5, 2), (49, "Henrique", 5, 2), (50, "Gustavo", 5, 2), (51, "Felipe", 6, 2), (52, "Lucas", 6, 2), (53, "Theo", 6, 2), (54, "Lorenzo", 6, 2), (55, "Benjamin", 6, 2), (56, "Davi", 6, 2), (57, "João", 6, 2), (58, "Guilherme", 6, 2), (59, "Murilo", 6, 2), (60, "Heitor", 6, 2), (61, "Arthur", 7, 2), (62, "Pedro", 7, 2), (63, "Gabriel", 7, 2), (64, "Enzo", 7, 2), (65, "Matheus", 7, 2), (66, "Lorenzo", 7, 2), (67, "Davi", 7, 2), (68, "Benjamin", 7, 2), (69, "João", 7, 2), (70, "Guilherme", 7, 2), (71, "Murilo", 8, 2), (72, "Luiz Felipe", 8, 2), (73, "Benjamin", 8, 2), (74, "Guilherme", 8, 2), (75, "Samuel", 8, 2), (76, "Luiza", 9, 6)]

disciplinas_ex :: [Disciplina]
disciplinas_ex = [(1, 2, "Engenharia de Software", 4), (2, 2, "Programação Orientada a Objetos", 4), (3, 2, "Banco de Dados", 4), (4, 2, "Sistemas Operacionais", 4), (5, 2, "Redes de Computadores", 4), (6, 2, "Programação Web", 4), (7, 2, "Programação para Dispositivos Móveis", 4), (8, 2, "Programação para Dispositivos Móveis", 4), (9, 2, "Programação para Dispositivos Móveis", 4), (10, 2, "Programação para Dispositivos Móveis", 4), (11, 2, "Programação para Dispositivos Móveis", 4), (12, 2, "Programação para Dispositivos Móveis", 4), (13, 2, "Programação para Dispositivos Móveis", 4), (14, 2, "Programação para Dispositivos Móveis", 4), (15, 2, "Programação para Dispositivos Móveis", 4), (16, 2, "Programação para Dispositivos Móveis", 4), (17, 2, "Programação para Dispositivos Móveis", 4), (18, 2, "Programação para Dispositivos Móveis", 4), (19, 2, "Programação para Dispositivos Móveis", 4), (20, 2, "Programação para Dispositivos Móveis", 4), (21, 2, "Programação para Dispositivos Móveis", 4), (22, 2, "Programação para Dispositivos Móveis", 4), (23, 2, "Programação para Dispositivos Móveis", 4), (24, 2, "Programação para Dispositivos Móveis", 4), (25, 2, "Programação para Dispositivos Móveis", 4), (26, 2, "Programação para Dispositivos Móveis", 4), (27, 2, "Programação para Dispositivos Móveis",7) ]


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
    addCurso curso
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

getNomeCurso((_, x, _)) = x

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


getNomeAluno((_, x, _, _)) = x
getCodCursoAluno((_, _, x, _)) = x
getPeriodoAluno((_, _, _, x)) = x


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


qntAlunosPorCurso([], []) = putStrLn " "
qntAlunosPorCurso(c:qnts, c2:cursos) = do
    putStrLn ("Quantidade de alunos no curso " ++ snd3(c2) ++ ": " ++ show c)
    qntAlunosPorCurso(qnts, cursos)

gestaoStaticsAlunos alunos cursos = do
    let qntAlunos = length alunos
    let qntAlunosCurso = [length [x | x <- alunos, getCodCursoAluno(x) == fst3(y)] | y <- cursos]
    let qntAlunosPeriodo = [length [x | x <- alunos, getPeriodoAluno(x) == y] | y <- [1..12]]
    putStrLn ("Quantidade de alunos cadastrados: " ++ show qntAlunos)
    putStrLn " "
    qntAlunosPorCurso(qntAlunosCurso, cursos)
    putStrLn ("Quantidade de alunos por período: " ++ show qntAlunosPeriodo)
    return alunos

buscaAluno :: ([Aluno], Matricula) -> [Aluno]
buscaAluno(alunos, matricula) = [x | x <- alunos, fst4(x) == matricula]

alunosDoCurso :: ([Aluno], Codigo) -> [Aluno]
alunosDoCurso(alunos, codCurso) = [x | x <- alunos, getCodCursoAluno(x) == codCurso]

alunosDoPeriodo :: ([Aluno], Periodo) -> [Aluno]
alunosDoPeriodo(alunos, periodo) = [x | x <- alunos, getPeriodoAluno(x) == periodo]

apresentaAlusDoCurso :: ([Aluno], [Curso]) -> IO [Aluno]
apresentaAlusDoCurso(alunos, cursos) = do
    putStr "Digite o código do curso: "
    cod <- getLine
    let aux = alunosDoCurso(alunos, read cod)
    let curso = buscaCurso(cursos, read cod)

    putStrLn " "

    if (aux == [])
        then putStrLn "Não há alunos cadastrados nesse curso"
    else do
        putStrLn ("------------- " ++ show(length aux) ++ " ALUNOS DO CURSO " ++ getNomeCurso(curso !! 0) ++ ": ---------------")
        apresentaAlunos aux cursos
    
    return alunos

apresentaAlusDoPeriodo(alunos, cursos) = do
    putStr "Digite o período: "
    periodo <- getLine
    let aux = alunosDoPeriodo(alunos, read periodo)

    if (aux == [])
        then putStrLn "Não há alunos cadastrados nesse período"
    else do
        putStrLn ("------------- " ++ show(length aux) ++ " ALUNOS DO PERÍODO " ++ periodo ++ ": ---------------")
        apresentaAlunos aux cursos
    
    return alunos

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

getCodCursoDisciplina((_, x, _, _)) = x
getNomeDisc((_, _, x, _)) = x
getPeriodoDisciplina((_, _, _, x)) = x

{- gestaoLerDisciplina :: IO () -}
gestaoLerDisciplina disciplinas cursos = do
    disciplina <- lerDisciplina
    let cod = getCodCursoDisciplina(disciplina)
    let aux = buscaCurso(cursos, cod)

    if (aux == [])
        then do
            putStrLn "\nCurso não cadastrado\nPor favor, digite um codigo valido!\n"
            gestaoLerDisciplina disciplinas cursos
    else do
        putStrLn ("Disciplina: " ++ show disciplina)
        return (disciplina:disciplinas)


apresentaDisciplinas [] cursos = putStrLn " "
apresentaDisciplinas ((codigo, codigoCurso, nome, periodo):t) cursos = do
    let c = buscaCurso(cursos, codigoCurso)
    putStrLn ("Código da disciplina:............ " ++ show codigo)
    putStrLn ("Nome da disciplina:.............. " ++ nome)
    putStrLn ("Período da disciplina:........... " ++ show periodo)
    putStrLn ("Curso da disciplina:............. " ++ snd3(head(c)))
    putStrLn " "
    apresentaDisciplinas t cursos


gestaoApresentaDisciplina(disciplinas, cursos) = do
    if (disciplinas == [])
        then putStrLn "Não há disciplinas cadastradas"
    else do
        putStrLn ("------------- DISCIPLINAS CADASTRADAS ---------------")
        apresentaDisciplinas disciplinas cursos
    return disciplinas

buscaDisciplina(codigo, disciplinas) = [x | x <- disciplinas, fst4(x) == codigo]

disciDoCurso(codCurso, disciplinas) = [x | x <- disciplinas, getCodCursoDisciplina(x) == codCurso]

disciDoPeriodo(periodo, disciplinas) = [x | x <- disciplinas, getPeriodoDisciplina(x) == periodo]

apresentaDisciDoCurso(disciplinas, cursos) = do
    putStr "Digite o código do curso: "
    cod <- getLine
    let aux = disciDoCurso(read cod, disciplinas)
    let curso = buscaCurso(cursos, read cod)

    putStrLn " "

    if (aux == [])
        then putStrLn "Não há disciplinas cadastradas nesse curso"
    else do
        putStrLn ("------------- " ++ show(length aux) ++ " DISCIPLINAS DO CURSO " ++ getNomeCurso(curso !! 0) ++ ": ---------------")
        apresentaDisciplinas aux cursos
    
    return disciplinas

apresentaDisciDoPeriodo(disciplinas, cursos) = do
    putStr "Digite o período: "
    periodo <- getLine
    let aux = disciDoPeriodo(read periodo, disciplinas)

    if (aux == [])
        then putStrLn "Não há disciplinas cadastradas nesse período"
    else do
        putStrLn ("------------- " ++ show(length aux) ++ " DISCIPLINAS DO PERÍODO " ++ periodo ++ ": ---------------")
        apresentaDisciplinas aux cursos
    
    return disciplinas


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

    putStr "\nDeseja digitar a segunda nota (S/N): "
    op <- getLine

    if (op == "S" || op == "s")
        then do
            putStr "Digite a nota 2 do aluno: "
            nota2 <- getLine
            return (read matricula, read codigo, read nota1, read nota2)
    else do
        return (read matricula, read codigo, read nota1, -1)

{- gestaoLerNotas :: IO () -}
gestaoLerNotas notas alunos disciplinas cursos  = do
    nota <- lerNotas
    let aux = buscaAluno(alunos, fst4(nota))
    let aux2 = buscaDisciplina(snd4(nota), disciplinas)

    if (aux == [])
        then do
            putStrLn "\nAluno não cadastrado\nPor favor, digite uma matricula valida!\n"
            gestaoLerNotas notas alunos disciplinas cursos
    else if (aux2 == [])
        then do
            putStrLn "\nDisciplina não cadastrada\nPor favor, digite um codigo valido!\n"
            gestaoLerNotas notas alunos disciplinas cursos
    else do
        putStrLn ("Notas: " ++ show notas)
        return (nota:notas)

apresentaNotas :: [Notas] -> [Aluno] -> [Disciplina] -> [Curso] -> IO ()
apresentaNotas [] alunos disciplinas cursos = putStrLn " "
apresentaNotas ((matricula, codigo, nota1, nota2):t) alunos disciplinas cursos = do
    let a = buscaAluno(alunos, matricula)
    let d = buscaDisciplina(codigo, disciplinas)
    putStrLn ("Matrícula do aluno:.............. " ++ show matricula)
    putStrLn ("Nome do aluno:................... " ++ getNomeAluno(a!!0) )
    putStrLn ("Código da disciplina:............ " ++ show codigo)
    putStrLn ("Nome da disciplina:.............. " ++ getNomeDisc(d!!0) )
    putStrLn ("Nota 1:.......................... " ++ show nota1)
    putStrLn ("Nota 2:.......................... " ++ show nota2)
    putStrLn " "
    apresentaNotas t alunos disciplinas cursos 

gestaoApresentaNotas :: [Notas] -> [Aluno] -> [Disciplina] -> [Curso] -> IO [Notas]
gestaoApresentaNotas notas alunos disciplinas cursos = do
    if (notas == [])
        then putStrLn "Não há notas cadastradas"
    else do
        putStrLn ("------------- NOTAS CADASTRADAS ---------------")
        apresentaNotas notas alunos disciplinas cursos
    return notas


buscaNotaAluno(matricula, notas) = [x | x <- notas, fst4(x) == matricula]

apresentaNotasAluno :: (Matricula, [Notas], [Aluno], [Disciplina], [Curso]) -> IO ()
apresentaNotasAluno(matricula, notas, alunos, disciplinas, cursos) = do
    let aux = buscaNotaAluno(matricula, notas)
    if (aux == [])
        then putStrLn "Não há notas cadastradas para esse aluno"
    else do
        putStrLn ("------------- " ++ show(length aux) ++ " NOTAS DO ALUNO " ++ show matricula ++ ": ---------------")
        apresentaNotas aux alunos disciplinas cursos

gestaoApresentaNotasAluno notas alunos disciplinas cursos = do
    putStr "Digite a matrícula do aluno: "
    matricula <- getLine
    let aux = buscaAluno(alunos, read matricula)
    if (aux == [])
        then putStrLn "Aluno não cadastrado"
    else do
        apresentaNotasAluno(read matricula, notas, alunos, disciplinas, cursos)
    return notas


-- ----------------------------- MENU ----------------------------- --

-- Função menu que mostra as opções do programa

menu :: IO ()
menu = do
    putStrLn "\nMenu"
    putStrLn "1 - Ler Curso"
    putStrLn "2 - Apresentar Cursos"
    putStrLn "3 - Ler Aluno"
    putStrLn "4 - Apresentar Estatisticas Alunos"
    putStrLn "5 - Apresentar Alunos"
    putStrLn "6 - Ler Disciplina"
    putStrLn "7 - Apresentar Disciplinas"
    putStrLn "8 - Ler Notas"
    putStrLn "9 - Apresentar Notas"
    putStrLn "10 - Apresentar Alunos de um curso"
    putStrLn "11 - Apresentar Alunos de um periodo"
    putStrLn "12 - Apresentar disciplinas de um curso"
    putStrLn "13 - Apresentar disciplinas de um periodo"
    putStrLn "14 - Apresentar notas de um aluno"
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
executarAluno(4, alunos, cursos) = gestaoStaticsAlunos alunos cursos
executarAluno(5, alunos, cursos) = gestaoApresentaAlunos alunos cursos
executarAluno(10, alunos, cursos) = apresentaAlusDoCurso(alunos, cursos)
executarAluno(11, alunos, cursos) = apresentaAlusDoPeriodo(alunos, cursos)

executarDisciplina :: (Int, [Disciplina], [Curso]) -> IO [Disciplina]
executarDisciplina(6, disciplinas, cursos) = gestaoLerDisciplina disciplinas cursos
executarDisciplina(7, disciplinas, cursos) = gestaoApresentaDisciplina(disciplinas, cursos)
executarDisciplina(12, disciplinas, cursos) = apresentaDisciDoCurso(disciplinas, cursos)
executarDisciplina(13, disciplinas, cursos) = apresentaDisciDoPeriodo(disciplinas, cursos)

executarNotas :: (Int, [Notas], [Aluno], [Disciplina], [Curso]) -> IO [Notas]
executarNotas(8, notas, alunos, disciplinas, cursos) = gestaoLerNotas notas alunos disciplinas cursos
executarNotas(9, notas, alunos, disciplinas, cursos) = gestaoApresentaNotas notas alunos disciplinas cursos
executarNotas(14, notas, alunos, disciplinas, cursos) = gestaoApresentaNotasAluno notas alunos disciplinas cursos 

-- Função que executa o menu
executarMenu :: [Curso] -> [Aluno] -> [Disciplina] -> [Notas] -> IO ()
executarMenu cursos alunos disciplinas notas = do
    menu
    opcao <- lerOpcao
    if opcao == 1 || opcao == 2 then do
        aux <- executarCurso(opcao, cursos)
        executarMenu aux alunos disciplinas notas

    else if opcao == 3 || opcao == 4 || opcao == 5 then do
        aux2 <- executarAluno(opcao, alunos, cursos)
        executarMenu cursos aux2 disciplinas notas

    else if opcao == 6 || opcao == 7 || opcao == 12 || opcao == 13 then do
        aux3 <- executarDisciplina(opcao, disciplinas, cursos)
        executarMenu cursos alunos aux3 notas
    
    else if opcao == 8 || opcao == 9 || opcao == 14 then do
        aux4 <- executarNotas(opcao, notas, alunos, disciplinas, cursos)
        executarMenu cursos alunos disciplinas aux4
    
    else if opcao == 10 || opcao == 11 then do
        aux5 <- executarAluno(opcao, alunos, cursos)
        executarMenu cursos alunos disciplinas notas


    else if opcao == 0 then do return ()

    else do
        putStrLn "Opção inválida"
        executarMenu cursos alunos disciplinas notas
   
main = do
    alunos <- abrirAlunos
    cursos <- abrirCursos
    disciplinas <- abrirDisciplinas
    notas <- abrirNotas
    
    print alunos
    print cursos
    print disciplinas
    print notas

    executarMenu cursos alunos disciplinas notas
    
