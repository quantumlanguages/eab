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

-- Booleanas binarias
bboolOPeration :: Instruction -> Instruction -> Instruction -> Instruction

-- Booleana unarias
uboolOperation :: Instruction -> Instruction -> Instruction

-- Comparaciones
relOperation :: Instruction -> Instruction -> Instruction -> Instruction

-- Manipulación de la pila
stackOPeration :: Stack -> Instruction -> Stack

-- Ejecuciones de instrucciones
execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)

-- Programas
type Program = [Instruction]

-- Ejecutar programa
executeProgram :: Program -> Stack -> Stack

-- Compilador (interprete)
compile :: Expr -> Program

-- Ejecutar (interpretar) una expresión
execute :: Expr -> Instruction