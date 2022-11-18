data Tree a = Node a Bool (Tree a) (Tree a) | Empty deriving (Show)

indent :: [String] -> [String]
indent x = map ("   "++) x

layoutTree :: Show a => Tree a -> [String]
layoutTree Empty = []
layoutTree (Node here truth left right)
            = indent (layoutTree right) ++ [show truth] ++[show here] ++ indent (layoutTree left)

prettyTree :: Show a => Tree a -> String
prettyTree = unlines.layoutTree