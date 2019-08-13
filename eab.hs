{--
Interprete de un lenguaje de expreiones aritméticas y boolenanas.
--}

-- Sintáxis
-- Expresiones del lenguaje
data Expr = N Int | T | F
        | Succ Expr | Pred Expr
        | Expr :+ Expr | Expr :- Expr
        | Expr :* Expr | Expr :/ Expr |  Expr :% Expr
        | Not Expr | Expr :& Expr | Expr :| Expr
        | Expr :> Expr | Expr :< Expr | Expr := Expr
        | Expr :^ Expr
        | Max Expr Expr | Min Expr Expr
        | Fact Expr

-- Instrucciones
data Instruction = I Int | B Bool
                | ADD | AND | DIV | Eq | EXEC | GET | Gt | Lt
                | MUL | NOT | POP | REM | SEL | SUB | SWAP | ES [Instruction]

-- Semántica
-- Estado de la máquina representado con una pila
type Stack = [Instruction]

-- Funciones para evaluar expresiones
-- Aritméticas
arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
arithOperation (I a) (I b) ADD = I (a + b)
arithOperation (I a) (I b) DIV = if b /= 0 then I (a / b) else error "Division by zero"
arithOperation (I a) (I b) REM = if b /= 0 then I (a % b) else error "Division by zero"
arithOperation (I a) (I b) MUL = I (a * b)
arithOperation (I a) (I b) SUB = I (a - b)
arithOperation _ _ _ = error "Invalid arithmetic parameters"

-- Booleanas binarias
bboolOPeration :: Instruction -> Instruction -> Instruction -> Instruction
bboolOPeration (B p) (B q) AND = B (p && q)
bboolOPeration _ _ = error "Invalid boolean parameter"

-- Booleana unarias
uboolOperation :: Instruction -> Instruction -> Instruction
uboolOperation (B p) NOT = B (!p)
uboolOPeration _ _ = error "Invalid boolean parameter"

-- Comparaciones
relOperation :: Instruction -> Instruction -> Instruction -> Instruction
relOperation (I a) (I b) Eq = B (a == b)
relOperation (I a) (I b) Gt = B (a > b)
relOperation (I a) (I b) Lt = B (a < b)
relOperation _ _ _ = error "Invalid comparaison parameters"

-- Manipulación de la pila
stackOperation :: Stack -> Instruction -> Stack
stackOperation xs x@(I a) = (x:xs)
stackOperation xs x@(B p) = (x:xs)
stackOperation (y:ys) POP = ys
stackOperation (x:y:ys) SWAP = (y:x:ys)
stackOperation (x:y:(B p):ys) SEL = if p then (x:ys) else (y:ys)
stackOperation ((I y):ys) GET = (y!!ys:ys)
stackOperation xs (ES is) = foldr (stackOperation xs) [] is

-- Ejecuciones de instrucciones
execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)

-- Programas
type Program = [Instruction]

-- Ejecutar programa
executeProgram :: Program -> Stack -> Stack
executeProgram pr st = stackOperation st (ES pr)

-- Compilador (interprete)
compile :: Expr -> Program

-- Ejecutar (interpretar) una expresión
execute :: Expr -> Instruction
execute e = executeProgram (compile e)