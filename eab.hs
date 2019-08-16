{--
Interprete de un lenguaje de expresiones aritméticas y boolenanas.
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
        arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
        arithOperation (I a) (I b) ADD = I (a + b)
        arithOperation (I a) (I b) DIV = if b /= 0 then I (div a b) else error "Division by zero"
        arithOperation (I a) (I b) REM = if b /= 0 then I (mod a b) else error "Division by zero"
        arithOperation (I a) (I b) MUL = I (a * b)
        arithOperation (I a) (I b) SUB = I (a - b)
        arithOperation _ _ _ = error "Invalid arithmetic parameters"

        -- Booleanas binarias
        bboolOPeration :: Instruction -> Instruction -> Instruction -> Instruction
        bboolOPeration (B p) (B q) AND = B (p && q)
        bboolOPeration _ _ _ = error "Invalid boolean parameterS"

        -- Booleana unarias
        uboolOperation :: Instruction -> Instruction -> Instruction
        uboolOperation (B p) NOT = B (not p)
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
        stackOperation (x:y:(B p):ys) SEL = if p then (y:ys) else (x:ys)
        stackOperation ((I y):ys) GET = ((ys!!y):ys)
        stackOperation xs x@(ES p) = (x:xs)
        stackOperation _ _ = error "Invalid stack"

        -- Ejecuciones de secuencias ejecutables (subrutinas)
        execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)
        execOperation pr ((ES sr):stk) EXEC = (sr ++ pr, stk)
        execOperation _ _ _ = error "Invalid exec operation"

        -- Programas
        type Program = [Instruction]

        -- Ejecutar programa
        executeProgram :: Program -> Stack -> Stack
        executeProgram pr st =

        -- Compilador (interprete)
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
        compile (Max e1 e2) = [I 3, GET, I 2, GET] ++ r1 ++ r2 ++ [Gt, SEL]
                              where r1 = compile e1; r2 = compile e2
        compile (Min e1 e2) = [I 3, GET, I 2, GET] ++ r1 ++ r2 ++ [Lt, SEL]
                              where r1 = compile e1; r2 = compile e2
        compile (Fact e1) = factComp (compile e1)

        -- Funciones auxiliares para compilar expresiones
        expComp :: Program -> Program -> Program
        expComp p1 p2 = p1 ++ p2 ++ [
                ES [I 2, GET, -- respaldar r2
                        I 1, Lt, -- calcular r2 < 1
                       ES [POP, POP, POP, I 1], -- caso base, regresa 1, elimina código extra
                        ES [SWAP, I 1, SUB, -- r2
                        I 2, GET, SWAP, -- preparando parametros para caso recursivo,
                        I 2, GET, -- preparando código de llamada recursiva
                        EXEC, -- llamada recursiva
                        SWAP, POP, -- eliminando copia de código de la llamada recursiva
                        MUL -- calculando r1^r2 = r1^{r2-1} * r1
                        ], -- caso recursivo
                         SEL, EXEC -- revisa r2 < 1 y selecciona bloque a ejecutar
                        ]
                , I 1, GET, EXEC -- llamada por primera vez a la función
                ]

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

        -- Ejecutar (evaluar) una expresión}
        execute :: Expr -> Instruction
        execute e = case s of
                      r:[] -> r
                      _ -> error "Invalid final value"
                    where s = executeProgram (compile e)
