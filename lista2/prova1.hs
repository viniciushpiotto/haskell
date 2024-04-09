acervo :: [(Isbn, Titulo, Reserva, Volumes)]
acervo = [(1, "Harry Potter", False, 10), (2, "Harry Potter 2", False, 1), (3, "Harry Potter 3", True, 2)]

emprestimo :: [(Matricula, Isbn)]
emprestimo = [("2023", 1), ("2001", 1), ("2022", 2)]

type Acervo = [(Isbn, Titulo, Reserva, Volumes)]
type Emprestimo = [(Matricula, Isbn)]
type Isbn = Int -- Isbn de um livro
type Volumes = Int -- quantidade no acervo
type Titulo = String -- título do livro
type Matricula = String -- matrícula do discente
type Reserva = Bool -- deve permanecer na biblioteca?

func_1 :: Isbn -> Acervo -> Bool
func_1 _ [] = False
func_1 i ((a,_,c,_):xs)
    | i == a = not (c)
    | otherwise = func_1 i xs

func_2 :: Isbn -> Emprestimo -> Int
func_2 _ [] = 0
func_2 i ((_,i2):xs)
    | i == i2 = 1 + func_2 i xs
    | otherwise = func_2 i xs

func_3 :: Isbn -> Acervo -> Int
func_3 _ [] = 0
func_3 i ((a,_,_,d):xs)
    | i == a = d
    | otherwise = func_3 i xs

func_4 :: Isbn -> Int
func_4 i
    | not (func_1 i acervo) = 0
    | otherwise = (func_3 i acervo) - (func_2 i emprestimo)

func_5 :: Matricula -> Isbn -> Emprestimo
func_5 m i
    | not (func_1 i acervo) || (func_4 i) == 0 = emprestimo
    | otherwise = [(m,i)] ++ emprestimo