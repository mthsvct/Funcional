
insereAluno(mat,nome,curso,periodo,[]) = [(mat,nome,curso,periodo)]
insereAluno(mat,nome,curso,periodo,(m1,n1,c1,p1):r)
    | mat < m1 = (mat,nome,curso,periodo):(m1,n1,c1,p1):r
    | mat == m1 = (m1,n1,c1,p1):r
    | otherwise = (m1,n1,c1,p1):insereAluno(mat,nome,curso,periodo,r)



        
