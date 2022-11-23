import CreateTree
import ShowTree

import Control.Monad.State
import Data.List

arvore = criaArvore [("(p>((pvq)>(p^q)))", False)]
(arvoreValidada, verdValidade, falsValidade, contraExemplo) = validateAux arvore [] []
validadaStr = toStringTree arvoreValidada
(No p bool p_esq p_dir) = criaArvore[("p", False)]


main :: IO ()
main = do
    putStrLn $ ShowTree.showTree validadaStr
    if null contraExemplo 
    then print "A fórmula é válida, não há contra-exemplos"
    else
        do 
            putStrLn ("A fórmula não é válida, um contra-exemplo é")
            print contraExemplo

validate :: ArvoreTableaux -> Bool
validate tree = valorVerdade
    where
        (contradicoes, strVerdade, strFalso, contraExemplo) = validateAux tree [] []
        valorVerdade = length (strVerdade `intersect` strFalso) /= 0

validateAux ::  CreateTree.ArvoreTableaux -> [String] -> [String] -> (CreateTree.ArvoreTableaux, [String], [String], [String])
validateAux Vazio [] [] = (Vazio, [], [], [])
validateAux Vazio strVerdade strFalso = (Vazio, strVerdade, strFalso, [])
validateAux (No str valorVerdade esq dir) strVerdade strFalso
    | esq == Vazio && dir == Vazio = trataFolha (No str valorVerdade esq dir) strVerdade strFalso
    | length str == 1 && valorVerdade = combina str valorVerdade (validateAux esq (str:strVerdade) strFalso) (validateAux dir (str:strVerdade) strFalso) 
    | length str == 1 && (not valorVerdade) = combina str valorVerdade (validateAux esq (str:strVerdade) (str:strFalso)) (validateAux dir strVerdade (str:strFalso)) 
    | otherwise = combina str valorVerdade (validateAux esq strVerdade strFalso) (validateAux dir strVerdade strFalso)
    
combina :: String -> Bool -> (CreateTree.ArvoreTableaux, [String], [String], [String]) -> (CreateTree.ArvoreTableaux, [String], [String], [String]) -> (CreateTree.ArvoreTableaux, [String], [String], [String])
combina str valorVerdade (esq, verdadeEsq, falsoEsq, contraEsq) (dir, verdadeDir, falsoDir, contraDir) = 
    ((No str valorVerdade esq dir), nub (verdadeEsq `union` verdadeDir),nub (falsoDir `union` falsoEsq), if (null contraEsq) then contraDir else contraEsq)

trataFolha :: CreateTree.ArvoreTableaux -> [String] -> [String] -> (CreateTree.ArvoreTableaux, [String], [String], [String])
trataFolha (No str valorVerdade esq dir) strVerdade strFalso
    | valorVerdade = 
        if length((nub (str:strVerdade)) `intersect` (nub strFalso)) == 0 
        then ((No str valorVerdade esq dir), (str:strVerdade), strFalso, nub ((str:strVerdade)++(map negated strFalso))) 
        else ((No str valorVerdade (No "X" False Vazio Vazio) Vazio), (str:strVerdade), strFalso, [])
    | (not valorVerdade) =  
        if length((nub strVerdade) `intersect` (nub (str:strFalso))) == 0 
        then ((No str valorVerdade esq dir), strVerdade, (str:strFalso), nub(strVerdade++(map negated (str:strFalso)))) 
        else ((No str valorVerdade (No "X" False Vazio Vazio) Vazio), strVerdade, (str:strFalso), [])

negated :: String -> String
negated str = '~':str


-- anotaArvoreComContradicao :: CreateTree.ArvoreTableaux -> CreateTree.ArvoreTableaux
    
