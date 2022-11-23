module CreateTree where

import ShowTree

--TODO: encontrar uma forma de colocar derivações sequencias uma embaixod da outra
data ArvoreTableaux = No String Bool (ArvoreTableaux) (ArvoreTableaux) | Vazio  deriving(Show, Eq)

criaArvore :: [(String, Bool)] -> ArvoreTableaux
criaArvore [] = Vazio
criaArvore ((formula, valorVerdade) : resto)
    | (esq == " ") && (dir == " ") = No formula valorVerdade (criaArvore resto) Vazio
    | (op == '>') && valorVerdade = No formula valorVerdade (criaArvore ([(esq, False)]++resto)) (criaArvore ([(dir, True)]++resto))
    | (op == '>') && (not valorVerdade) = No formula valorVerdade (criaArvore ([(esq, True),(dir, False)]++resto)) Vazio
    | (op == '^') && valorVerdade = No formula valorVerdade (criaArvore ([(esq, True),(dir, True)]++resto)) Vazio
    | (op == '^') && (not valorVerdade) = No formula valorVerdade (criaArvore ([(esq, False)]++resto)) (criaArvore ([(dir, False)]++resto))
    | (op == 'v') && valorVerdade = No formula valorVerdade (criaArvore ([(esq, True)]++resto)) (criaArvore ([(dir, True)]++resto))
    | (op == 'v') && (not valorVerdade) = No formula valorVerdade (criaArvore ([(esq, False),(dir, False)]++resto)) Vazio
    | (op == '~') && valorVerdade = No formula valorVerdade (criaArvore ([(dir, False)]++resto)) Vazio
    | (op == '~') && (not valorVerdade) = No formula valorVerdade (criaArvore ([(dir, True)]++resto)) Vazio
    where
        resp = readOperator formula "" 0
        (esq, op, dir) = resp

readOperator :: String -> String -> Int -> (String, Char, String)
readOperator [] _ _ = (" ", 'E', " ")
readOperator (c : t) accstr accpar
    | (c == '>' || c == '^' || c == 'v' || c == '~') && (accpar == 1) = (accstr, c, t)
    | (c == '(') && (accpar == 0) = readOperator (init t) accstr (accpar+1) 
    | (c == '(') && (accpar > 0) = readOperator t (accstr ++ [c]) (accpar+1)
    | (c == ')') = readOperator t (accstr ++ [c]) (accpar-1)
    | c == ' ' = readOperator t accstr accpar
    | otherwise = readOperator t (accstr ++ [c]) accpar

toStringTree :: ArvoreTableaux -> Tree String
toStringTree Vazio = Empty
toStringTree (No here truth left right) = 
    Node ((show truth)++ ":" ++ here) (toStringTree left) (toStringTree right)
