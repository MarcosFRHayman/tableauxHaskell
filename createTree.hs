data Tree a = Node a Bool Int (Tree a) (Tree a) | Empty deriving (Show)

makeTree :: String -> Tree Char
makeTree [] = Empty
makeTree (a : t) = Node a True 2 (makeTree t) (makeTree t)

readOperator :: String -> String -> Int -> (String, String, String)
readOperator (c : t) accstr accpar
    | (c == '>' || c == '^' || c == 'v' || c == '~') && (accpar == 1) = (accstr, [c], t)
    | (c == '(') && (accpar == 0) = readOperator (init t) accstr (accpar+1) 
    | (c == '(') && (accpar > 0) = readOperator t (accstr ++ [c]) (accpar+1)
    | (c == ')') = readOperator t (accstr ++ [c]) (accpar-1)
    | c == ' ' = readOperator t accstr accpar
    | otherwise = readOperator t (accstr ++ [c]) accpar
    