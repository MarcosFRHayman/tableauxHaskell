data Tree a = Node a Bool Int (Tree a) (Tree a) | Empty deriving (Show)

makeTree :: String -> Tree Char
makeTree [] = Empty
makeTree (a : t) = Node a True (makeTree t) (makeTree t)