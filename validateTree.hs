import CreateTree
import ShowTree

import Control.Monad.State
import Data.List

-- data Tree a = Node a Bool Int (Tree a) (Tree a) | Empty deriving (Show)

arvore = criaArvore [("(p>(pvq))", False)]
arvoreStr = toStringTree arvore
(arvoreValidada, _, _) = validateAux arvore [] []
validadaStr = toStringTree arvoreValidada

main :: IO ()
main = do
    putStrLn . fst $ runState (ShowTree.simpleShow arvoreStr) 0 
    putStrLn $ ShowTree.showTree arvoreStr
    putStrLn $ ShowTree.showTree validadaStr

validate :: ArvoreTableaux -> Bool
validate tree = valorVerdade
    where
        (contradicoes, strVerdade, strFalso) = validateAux tree [] []
        valorVerdade = length (strVerdade `intersect` strFalso) /= 0

validateAux ::  CreateTree.ArvoreTableaux -> [String] -> [String] -> (CreateTree.ArvoreTableaux, [String], [String])
validateAux Vazio [] [] = (Vazio, [], [])
validateAux Vazio strVerdade strFalso = (Vazio, strVerdade, strFalso)
validateAux (No str valorVerdade Vazio Vazio) strVerdade strFalso = if length(strVerdade `intersect` strFalso) == 0 then ((No str valorVerdade Vazio Vazio), strVerdade, strFalso) else ((No str valorVerdade (No "X" False Vazio Vazio) Vazio), strVerdade, strFalso)
-- validateAux Vazio strVerdade strFalso = if length(strVerdade `intersect` strFalso) == 0 then (Vazio, strVerdade, strFalso) else ((No "X" False Vazio Vazio), strVerdade, strFalso)
validateAux (No str valorVerdade esq dir) strVerdade strFalso
    | length str == 1 && valorVerdade = combina str valorVerdade (validateAux esq (str:strVerdade) strFalso) (validateAux dir (str:strVerdade) strFalso) 
    | length str == 1 && (not valorVerdade) = combina str valorVerdade (validateAux esq (str:strVerdade) (str:strFalso)) (validateAux dir strVerdade (str:strFalso)) 
    | esq == Vazio && dir == Vazio = if length(strVerdade `intersect` strFalso) == 0 then ((No str valorVerdade esq dir), strVerdade, strFalso) else ((No str valorVerdade (No "X" False Vazio Vazio) Vazio), strVerdade, strFalso)
    | otherwise = combina str valorVerdade (validateAux esq strVerdade strFalso) (validateAux dir strVerdade strFalso)
    
combina :: String -> Bool -> (CreateTree.ArvoreTableaux, [String], [String]) -> (CreateTree.ArvoreTableaux, [String], [String]) -> (CreateTree.ArvoreTableaux, [String], [String])
combina str valorVerdade (esq, verdadeEsq, falsoEsq) (dir, verdadeDir, falsoDir) = ((No str valorVerdade esq dir), verdadeEsq, verdadeDir)

-- anotaArvoreComContradicao :: CreateTree.ArvoreTableaux -> CreateTree.ArvoreTableaux
    
