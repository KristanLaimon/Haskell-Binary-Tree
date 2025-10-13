import GHC.Exts.Heap (GenClosure(key))
data Arbol = Null | Nodo Integer Arbol Arbol deriving (Show)

insertArbol :: Integer -> Arbol -> Arbol
insertArbol x Null = Nodo x Null Null
insertArbol x (Nodo v izq der)
  | x < v = Nodo v (insertArbol x izq) der
  | x >= v = Nodo v izq (insertArbol x der)


myTree = insertArbol 1 Null


myAdvancedTree = insertArbol 1 $ insertArbol 5 $ insertArbol 4 Null


