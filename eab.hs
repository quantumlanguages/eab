{--
Interprete de un lenguaje de expresiones aritméticas y boolenanas.
Práctica 1
Autores.
Edgar Quiroz Castañeda
Sandra del Mar Soto Corderi
--}
module EAB where
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
                deriving (Show)

        -- Instrucciones
        data Instruction = I Int | B Bool
                        | ADD | AND | DIV | Eq | EXEC | GET | Gt | Lt
                        | MUL | NOT | POP | REM | SEL | SUB | SWAP | ES [Instruction]
                deriving (Show)

        -- Semántica
        -- Estado de la máquina representado con una pila
        type Stack = [Instruction]

        -- Funciones para evaluar expresiones
        -- Aritméticas
        --1. Funcion que realiza las operaciones de las instrucciones aritmeticas
        --Los primeros dos argumentos corresponden a los operandos y el tercero al operador.
        --La funcion devuelve una literal entera
        arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
        arithOperation (I a) (I b) ADD = I (a + b)
        arithOperation (I a) (I b) DIV = if b /= 0 then I (div a b) else error "Division by zero"
        arithOperation (I a) (I b) REM = if b /= 0 then I (mod a b) else error "Division by zero"
        arithOperation (I a) (I b) MUL = I (a * b)
        arithOperation (I a) (I b) SUB = I (a - b)
        arithOperation _ _ _ = error "Invalid arithmetic parameters"

        -- Booleanas binarias
        --2. Funcion que realiza las operaciones de las instrucciones booleanas binarias
        --Los primeros dos argumentos corresponden a los operandos y el tercero al operador.
        --La funcion devuelve una literal booleana
        bboolOperation :: Instruction -> Instruction -> Instruction -> Instruction
        bboolOperation (B p) (B q) AND = B (p && q)
        bboolOperation _ _ _ = error "Invalid boolean parameterS"

        -- Booleana unarias
        --3.Funcion que realiza las operaciones de las instrucciones booleanas unarias
        --El primer argumento corresponde al operando y el segundo al operador.
        --La funcion devuelve una literal booleana
        uboolOperation :: Instruction -> Instruction -> Instruction
        uboolOperation (B p) NOT = B (not p)
        uboolOperation _ _ = error "Invalid boolean parameter"

        -- Comparaciones
        --4.Funcion que realiza las operaciones de las instrucciones relaciones
        --Los primeros dos argumentos corresponden a los operandos y el tercero al operador.
        --La funcion devuelve una literal booleana
        relOperation :: Instruction -> Instruction -> Instruction -> Instruction
        relOperation (I a) (I b) Eq = B (a == b)
        relOperation (I a) (I b) Gt = B (a > b)
        relOperation (I a) (I b) Lt = B (a < b)
        relOperation _ _ _ = error "Invalid comparaison parameters"

        -- Manipulación de la pila
        --5. Funcion que realiza las operaciones de las instrucciones que alteran la pila.
        --La funcion devuelve una pila
        stackOperation :: Stack -> Instruction -> Stack
        stackOperation xs x@(I _) = (x:xs)
        stackOperation xs x@(B _) = (x:xs)
        stackOperation stk POP =
                case stk of
                        (_:ys) -> ys
                        _ -> error "Not enough paramenters for a pop"
        stackOperation stk SWAP =
                case stk of
                        (x:y:ys) -> (y:x:ys)
                        _ -> error "Not enough paramenters for a swap"
        stackOperation stk SEL =
                case stk of
                        (x:y:(B p):ys) -> if p then (y:ys) else (x:ys)
                        (x:y:z:ys) -> error "Invalid boolean value in sel"
                        _ -> error "Not enough parameters for a sel"
        stackOperation ((I y):ys) GET =
                if 0 <= k && k < length ys
                  then (ys!!k:ys)
                  else error "Index not found"
                where k = y-1
        stackOperation xs x@(ES _) = (x:xs)
        stackOperation _ _ = error "Invalid stack"

        -- Ejecuciones de secuencias ejecutables (subrutinas)
        --6. Funcion que devuelve la secuencia de instrucciones y la pila resultante al llamar exec.
        execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)
        execOperation pr ((ES sr):stk) EXEC = (sr ++ pr, stk)
        execOperation _ _ _ = error "Invalid exec operation"

        -- Programas
        type Program = [Instruction]

        -- Ejecutar programa
        --7. Funcion que dada una secuencia de instrucciones y una pila de valores
        -- Devuelve la pila de valores resultantes después de ejecutar las instrucciones.
        executeProgram :: Program -> Stack -> Stack
        executeProgram (ADD:xs) (n:m:ys) = executeProgram xs  ((arithOperation m n ADD) : ys)
        executeProgram (DIV:xs) (n:m:ys) = executeProgram xs  ((arithOperation m n DIV) : ys)
        executeProgram (REM:xs) (n:m:ys) = executeProgram xs  ((arithOperation m n REM) : ys)
        executeProgram (MUL:xs) (n:m:ys) = executeProgram xs  ((arithOperation m n MUL) : ys)
        executeProgram (SUB:xs) (n:m:ys) = executeProgram xs  ((arithOperation m n SUB) : ys)
        executeProgram (AND:xs) (n:m:ys) = executeProgram xs  ((bboolOperation n m AND) : ys)
        executeProgram (Eq:xs) (n:m:ys) = executeProgram xs  ((relOperation m n Eq) : ys)
        executeProgram (Gt:xs) (n:m:ys) = executeProgram xs  ((relOperation m n Gt) : ys)
        executeProgram (Lt:xs) (n:m:ys) = executeProgram xs  ((relOperation m n Lt) : ys)
        executeProgram (NOT:xs) (n:ys) = executeProgram xs  ((uboolOperation n NOT) : ys)
        executeProgram (POP:xs) ys = executeProgram xs (stackOperation ys POP)
        executeProgram ((I n):xs) ys = executeProgram xs (stackOperation ys (I n))
        executeProgram ((B b):xs) ys = executeProgram xs (stackOperation ys (B b))
        executeProgram (SWAP:xs) ys = executeProgram xs (stackOperation ys SWAP)
        executeProgram (SEL:xs) ys = executeProgram xs  (stackOperation ys SEL)
        executeProgram (GET:xs) ys = executeProgram xs (stackOperation ys GET)
        executeProgram ((ES es): xs) ys = (executeProgram (xs) ((stackOperation ys (ES es))))
        executeProgram ((EXEC): xs) ys = (executeProgram xs' ys') where (xs', ys') = execOperation xs ys EXEC
        executeProgram [] ys = ys
        executeProgram _ _ = error "Invalid operation structure"

        -- Compilador (interprete)
        --8. Funcion que dada una expresión aritmético-booleana
        -- Obtiene la secuencia de instrucciones que permite evaluarla
        compile :: Expr -> Program
        compile (N n) = [I n]
        compile T = [B True]
        compile F = [B False]
        compile (Succ e) = (compile e) ++ [I 1, ADD]
        compile (Pred e) = (compile e) ++ [I 1, SUB]
        compile (e1 :+ e2) = (compile e1) ++ (compile e2) ++ [ADD]
        compile (e1 :- e2) = (compile e1) ++ (compile e2) ++ [SUB]
        compile (e1 :* e2) = (compile e1) ++ (compile e2) ++ [MUL]
        compile (e1 :/ e2) = (compile e1) ++ (compile e2) ++ [DIV]
        compile (e1 :% e2) = (compile e1) ++ (compile e2) ++ [REM]
        compile (Not e) = (compile e) ++ [NOT]
        compile (e1 :& e2) = (compile e1) ++ (compile e2) ++ [AND]
        compile (e1 :| e2) = compile (Not ((Not e1) :& (Not e2)))
        compile (e1 :< e2) = (compile e1) ++ (compile e2) ++ [Lt]
        compile (e1 :> e2) = (compile e1) ++ (compile e2) ++ [Gt]
        compile (e1 := e2) = (compile e1) ++ (compile e2) ++ [Eq]
        compile (e1 :^ e2) = expComp (compile e1) (compile e2)
        compile (Max e1 e2) = compr (compile e1) (compile e2) Gt
        compile (Min e1 e2) = compr (compile e1) (compile e2) Lt
        compile (Fact e1) = factComp (compile e1)

        -- Funciones auxiliares para compilar expresiones
        -- Funcion auxiliar que nos permite compilar la comparación de dos elementos
        compr :: Program -> Program -> Instruction -> Program
        compr r1 r2 ins = r2 ++ r1 -- Añadiendo programa de las expresiones
                          ++ [I 2, GET, I 2, GET] -- Respaldando valores
                          ++ [ins] -- Instrucción de comparación
                          ++ [I 3, GET, I 3, GET] -- Preparando parametros para la selección
                          ++ [
                            SEL, -- selección
                            SWAP, POP, SWAP, POP -- Eliminando respaldo de valores
                          ]

        -- Funcion auxiliar que nos permite compilar la exponenciaciación.
        expComp :: Program -> Program -> Program
        expComp p1 p2 = p1 ++ p2 ++ [
                ES [I 2, GET, -- respaldar r2
                        I 1, Lt, -- calcular r2 < 1
                        ES [POP, POP, POP, I 1], -- caso base, regresa 1, elimina código extra
                        ES [SWAP, I 1, SUB, -- r2
                        I 3, GET, SWAP, -- preparando parametros para caso recursivo,
                        I 3, GET, -- respaldar el segundo bloque
                        I 1, GET, -- obtiene el que se ejecutará
                        EXEC, -- llamada recursiva
                        SWAP, POP, -- eliminando copia de código de la llamada recursiva
                        MUL -- calculando r1^r2 = r1^{r2-1} * r1
                        ], -- caso recursivo
                        SEL, EXEC -- revisa r2 < 1 y selecciona bloque a ejecutar
                        ]
                , I 1, GET, EXEC -- llamada por primera vez a la función
                ]

        -- Funcion auxiliar que nos permite compilar el factorial de un numero
        factComp :: Program -> Program
        factComp p = p ++ [
                ES [I 2, GET, -- respaldar r
                        I 1, Lt, -- calcular r < 1
                        ES [POP, POP, I 1], -- caso base, regresa 1, elimina código extra
                        ES [I 2, GET, I 1, SUB, -- r2
                        I 2, GET, -- preparando parametros para caso recursivo,
                        I 1, GET, -- preparando código de llamada recursiva
                        EXEC, -- llamada recursiva
                        SWAP, POP, -- eliminando copia de código de la llamada recursiva
                        MUL -- calculando fac r = fac (r-1) * r
                        ], -- caso recursivo
                         SEL, EXEC -- revisa r2 < 1 y selecciona bloque a ejecutar
                        ]
                , I 1, GET, EXEC -- llamada por primera vez a la función
                ]

        -- Ejecutar (evaluar) una expresión
        --9. Función que dada una expresión aritmético-booleana, realiza la evaluación del Programa
        --Devuelve una literal entera o una literal booleana.
        execute :: Expr -> Instruction
        execute e = case s of
                      r:[] -> r
                      _ -> error "Invalid final value"
                    where s = executeProgram (compile e) []
