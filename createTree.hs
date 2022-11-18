data Tree a = Node a Bool (Tree a) (Tree a) | Empty deriving (Show)

createTree :: [(String, Bool)] -> Tree String
createTree [] = Empty
createTree ((formula, valorVerdade) : resto)
    | (esq == " ") && (dir == " ") = Node formula valorVerdade (createTree resto) Empty
    | (op == '>') && valorVerdade = Node formula valorVerdade (createTree (resto++[(esq, False)])) (createTree (resto++[(dir, True)]))
    | (op == '>') && (not valorVerdade) = Node formula valorVerdade (createTree (resto++[(esq, True),(dir, False)])) Empty
    | (op == '^') && valorVerdade = Node formula valorVerdade (createTree (resto++[(esq, True),(dir, True)])) Empty
    | (op == '^') && (not valorVerdade) = Node formula valorVerdade (createTree (resto++[(esq, False)])) (createTree (resto++[(dir, False)]))
    | (op == 'v') && valorVerdade = Node formula valorVerdade (createTree (resto++[(esq, True)])) (createTree (resto++[(dir, True)]))
    | (op == 'v') && (not valorVerdade) = Node formula valorVerdade (createTree (resto++[(esq, False),(dir, False)])) Empty
    | (op == '~') && valorVerdade = Node formula valorVerdade (createTree (resto++[(dir, False)])) Empty
    | (op == '~') && (not valorVerdade) = Node formula valorVerdade (createTree (resto++[(dir, True)])) Empty
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

indent :: [String] -> [String]
indent x = map ("     "++) x

layoutTree :: Show a => Tree a -> [String]
layoutTree Empty = []
layoutTree (Node here truth left right)
            = indent (layoutTree right) ++ [show truth] ++[show here] ++ indent (layoutTree left)

prettyTree :: Show a => Tree a -> String
prettyTree = unlines.layoutTree