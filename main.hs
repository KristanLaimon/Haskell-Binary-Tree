-- File: main.hs
import MenuHelper (displayMenuAndGetChoice)
import BinaryTree (
    Node(..), 
    nodeInsertInto, 
    nodeToString, 
    nodeSearchValueFrom, 
    nodeCreateFromList
  )
import BinaryTreeDefaults (exampleRootNode)
import System.IO (hFlush, stdout,hSetEncoding, utf8, utf16)
import Text.Read (readMaybe)

menuOptions :: [String]
menuOptions = [
    "1. Insertar valor",
    "2. Buscar valor",
    "3. Recorrer (Imprimir árbol)",
    "4. Sumar valores del árbol",
    "5. Encontrar valor Máximo",
    "6. Contar nodos",
    "7. Eliminar un nodo",
    "8. Salir"
  ]

-- | The main application loop.
-- | It takes the current state of the tree, shows the menu,
-- | and then calls itself recursively with the (potentially) new tree state.
mainLoop :: Node -> IO ()
mainLoop currentNode = do
    choice <- displayMenuAndGetChoice menuOptions
    
    case choice of
        1 -> handleInsert currentNode
        2 -> handleSearch currentNode
        3 -> handleTraverse currentNode
        4 -> handleSum currentNode
        5 -> handleMax currentNode
        6 -> handleCount currentNode
        7 -> handleDelete currentNode
        8 -> putStrLn "\n¡Adiós!" >> return ()
        _ -> mainLoop currentNode -- Should be unreachable due to MenuHelper, just in case

-- | Handler for option 1: Insert
handleInsert :: Node -> IO ()
handleInsert currentNode = do
    putStr "== 1. Insertar ==\nIngresa el valor numérico a insertar: "
    hFlush stdout
    input <- getLine
    let maybeVal = readMaybe input :: Maybe Integer
    
    case maybeVal of
        Nothing -> do
            putStrLn "[Error] Entrada no válida. Debes ingresar un número."
            mainLoop currentNode
        Just val -> do
            let newNode = nodeInsertInto currentNode val
            putStrLn $ "Valor " ++ show val ++ " insertado."
            putStrLn "Nuevo árbol:"
            putStrLn $ nodeToString newNode
            mainLoop newNode -- Continue loop with the new tree

-- | Handler for option 2: Search
handleSearch :: Node -> IO ()
handleSearch currentNode = do
    putStr "== 2. Buscar ==\nIngresa el valor numérico a buscar: "
    hFlush stdout
    input <- getLine
    let maybeVal = readMaybe input :: Maybe Integer
    
    case maybeVal of
        Nothing -> do
            putStrLn "[Error] Entrada no válida. Debes ingresar un número."
            mainLoop currentNode
        Just val -> do
            let mFoundNode = nodeSearchValueFrom currentNode val
            case mFoundNode of
                Nothing -> putStrLn $ "Valor " ++ show val ++ " NO encontrado."
                Just n  -> putStrLn $ "¡Valor " ++ show val ++ " encontrado!"
            mainLoop currentNode -- Continue loop with the same tree

-- | Handler for option 3: Traverse
handleTraverse :: Node -> IO ()
handleTraverse currentNode = do
    putStrLn "== 3. Recorrer (Preorder) =="
    putStrLn $ nodeToString currentNode
    mainLoop currentNode

-- | Handler for option 4: Sum
handleSum :: Node -> IO ()
handleSum currentNode = do
    putStrLn "== 4. Sumar =="
    putStrLn "Función no implementada."
    -- TODO: Implement nodeSum and call it here
    -- let total = nodeSum currentNode
    -- putStrLn $ "La suma de todos los nodos es: " ++ show total
    mainLoop currentNode

-- | Handler for option 5: Max
handleMax :: Node -> IO ()
handleMax currentNode = do
    putStrLn "== 5. Máximo =="
    putStrLn "Función no implementada."
    -- TODO: Implement nodeMax and call it here
    mainLoop currentNode

-- | Handler for option 6: Count
handleCount :: Node -> IO ()
handleCount currentNode = do
    putStrLn "== 6. Contar =="
    putStrLn "Función no implementada."
    -- TODO: Implement nodeCount and call it here
    mainLoop currentNode

-- | Handler for option 7: Delete
handleDelete :: Node -> IO ()
handleDelete currentNode = do
    putStrLn "== 7. Eliminar =="
    putStrLn "Función no implementada."
    -- TODO: Implement nodeDelete and call it here
    mainLoop currentNode

-- | Main entry point
main :: IO ()
main = do
    hSetEncoding stdout utf16 
    putStrLn "==========================================="
    putStrLn "Práctica: Árboles Binarios en Haskell"
    putStrLn "Cargando árbol de ejemplo..."
    -- We use the example tree from your BinaryTreeDefaults.hs file
    let rootNode = exampleRootNode
    mainLoop rootNode