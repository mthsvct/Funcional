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
    let novo = "(" ++ show matricula ++ ", " ++ nome ++ ", " ++ show codigo ++ ", " ++ show periodo ++ ")\n"
    appendFile arqAlunos novo
    
main = do
    alunos <- abrirAlunos
    print alunos
    
