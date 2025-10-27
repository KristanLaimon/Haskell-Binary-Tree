-- File: main.hs
import MenuHelper (displayMenuAndGetChoice, pressAnyKeyToContinue)
import BinaryTree (
    Node(..), 
    nodeCreateSimpleWithValue, 
    nodeInsertInto, 
    nodeToString, 
    nodeSearchValueFrom, 
    nodeCreateFromList,
    sumAllNodes,
    maxNode,
    countNodes,
    nodeDelete 
  )
import System.IO (hFlush, stdout, hSetEncoding, utf8, hSetBuffering, BufferMode(NoBuffering))
import Text.Read (readMaybe)

-- | The list of options for the main menu.
menuOptions :: [String]
menuOptions = [
    "1. Insertar valor",
    "2. Buscar valor",
    "3. Recorrer (Imprimir arbol)",
    "4. Sumar valores del arbol",
    "5. Encontrar valor Maximo",
    "6. Contar nodos",
    "7. Eliminar un nodo",
    "8. Salir"
  ]

-- | The main application loop.
-- | It now takes a 'Maybe Node' as the state,
-- | allowing the tree to start as 'Nothing' (empty).
mainLoop :: Maybe Node -> IO ()
mainLoop maybeCurrentNode = do
    choice <- displayMenuAndGetChoice menuOptions -- This clears the screen
    
    (newMaybeNode, shouldExit) <- case choice of
        1 -> handleInsert maybeCurrentNode
        2 -> handleSearch maybeCurrentNode
        3 -> handleTraverse maybeCurrentNode
        4 -> handleSum maybeCurrentNode
        5 -> handleMax maybeCurrentNode
        6 -> handleCount maybeCurrentNode
        7 -> handleDelete maybeCurrentNode
        8 -> handleExit maybeCurrentNode
        _ -> return (maybeCurrentNode, False) -- Should not reach there, just in case...
    
    if shouldExit
        then return () -- Exit
        else do
            pressAnyKeyToContinue 
            mainLoop newMaybeNode

-- | Handler for option 1: Insert
-- | This is the only function that can change the state from 'Nothing' to 'Just Node'.
handleInsert :: Maybe Node -> IO (Maybe Node, Bool)
handleInsert maybeCurrentNode = do
    putStr "== 1. Insertar ==\nIngresa el valor numerico a insertar: "
    input <- getLine
    let maybeVal = readMaybe input :: Maybe Integer
    
    case maybeVal of
        Nothing -> do
            putStrLn "[Error] Entrada no valida. Debes ingresar un numero."
            return (maybeCurrentNode, False) -- Return old state
        
        Just val -> do
            -- Case logic for insertion
            case maybeCurrentNode of
                -- If tree is empty ('Nothing'), create the first node.
                Nothing -> do
                    let newNode = nodeCreateSimpleWithValue val
                    putStrLn $ "arbol iniciado con el valor " ++ show val ++ "."
                    putStrLn $ nodeToString newNode
                    return (Just newNode, False) -- Return new state
                
                -- If tree exists, insert into it.
                Just node -> do
                    let newNode = nodeInsertInto node val
                    putStrLn $ "Valor " ++ show val ++ " insertado."
                    putStrLn "Nuevo arbol:"
                    putStrLn $ nodeToString newNode
                    return (Just newNode, False) -- Return new state

-- | Handler for option 2: Search
handleSearch :: Maybe Node -> IO (Maybe Node, Bool)
handleSearch maybeCurrentNode = do
    case maybeCurrentNode of
        Nothing -> putStrLn "El arbol esta vacio. No se puede buscar."
        Just node -> do
            putStr "== 2. Buscar ==\nIngresa el valor numerico a buscar: "
            input <- getLine
            let maybeVal = readMaybe input :: Maybe Integer
            
            case maybeVal of
                Nothing -> putStrLn "[Error] Entrada no valida. Debes ingresar un numero."
                Just val -> do
                    let mFoundNode = nodeSearchValueFrom node val
                    case mFoundNode of
                        Nothing -> putStrLn $ "Valor " ++ show val ++ " NO encontrado."
                        Just n  -> putStrLn $ "¡Valor " ++ show val ++ " encontrado!"
    
    return (maybeCurrentNode, False) -- Tree state does not change

-- | Handler for option 3: Traverse
handleTraverse :: Maybe Node -> IO (Maybe Node, Bool)
handleTraverse maybeCurrentNode = do
    putStrLn "== 3. Recorrer (Preorder) =="
    case maybeCurrentNode of
        Nothing -> putStrLn "El arbol esta vacio."
        Just node -> putStrLn $ nodeToString node
    return (maybeCurrentNode, False) -- Tree state does not change

-- | Handler for option 4: Sum
handleSum :: Maybe Node -> IO (Maybe Node, Bool)
handleSum maybeCurrentNode = do
    putStrLn "== 4. Sumar =="
    case maybeCurrentNode of
        Nothing -> putStrLn "El arbol esta vacio. La suma es 0."
        Just node -> do
            let total = sumAllNodes node 
            putStrLn $ "La suma de todos los nodos es: " ++ show total
    return (maybeCurrentNode, False) -- Tree state does not change

-- | Handler for option 5: Max
handleMax :: Maybe Node -> IO (Maybe Node, Bool)
handleMax maybeCurrentNode = do
    putStrLn "== 5. Maximo =="
    case maybeCurrentNode of
        Nothing -> putStrLn "El arbol esta vacio. No hay valor maximo."
        Just node -> do
            let maxVal = maxNode node
            putStrLn $ "El valor maximo en el arbol es: " ++ show maxVal
    return (maybeCurrentNode, False) -- Tree state does not change

-- | Handler for option 6: Count
handleCount :: Maybe Node -> IO (Maybe Node, Bool)
handleCount maybeCurrentNode = do
    putStrLn "== 6. Contar =="
    case maybeCurrentNode of
        Nothing -> putStrLn "El arbol esta vacio. El conteo es 0."
        Just node -> do
            let count = countNodes node
            putStrLn $ "El numero total de nodos es: " ++ show count
    return (maybeCurrentNode, False) -- Tree state does not change

-- | Handler for option 7: Delete
handleDelete :: Maybe Node -> IO (Maybe Node, Bool)
handleDelete maybeCurrentNode = do
    putStrLn "== 7. Eliminar =="
    case maybeCurrentNode of
        Nothing -> do
            putStrLn "El arbol esta vacio. No se puede eliminar."
            return (Nothing, False)
        
        Just node -> do
            putStr "Ingresa el valor numerico a eliminar: "
            input <- getLine
            let maybeVal = readMaybe input :: Maybe Integer

            case maybeVal of
                Nothing -> do
                    putStrLn "[Error] Entrada no valida. Debes ingresar un numero."
                    return (Just node, False)
                
                Just val -> do
                    -- Check if value exists before deleting
                    case nodeSearchValueFrom node val of
                        Nothing -> do
                            putStrLn $ "[Error] Valor " ++ show val ++ " no encontrado en el arbol."
                            return (Just node, False)
                        
                        Just _ -> do
                            -- Perform the deletion
                            let newMaybeNode = nodeDelete node val
                            putStrLn $ "Valor " ++ show val ++ " eliminado."
                            
                            case newMaybeNode of
                                Nothing -> do
                                    putStrLn "El arbol ha quedado vacio."
                                    return (Nothing, False)
                                Just newNode -> do
                                    putStrLn "Nuevo arbol:"
                                    putStrLn $ nodeToString newNode
                                    return (Just newNode, False)


-- | Handler for option 8: Exit
handleExit :: Maybe Node -> IO (Maybe Node, Bool)
handleExit maybeCurrentNode = do
    putStrLn "\n¡Adios!"
    return (maybeCurrentNode, True) -- Return True to signal exit

-- | Main entry point
main :: IO ()
main = do
    hSetEncoding stdout utf8 
    -- Disable buffering to ensure prompts appear immediately
    hSetBuffering stdout NoBuffering
    
    putStrLn "==========================================="
    putStrLn "Practica: arboles Binarios en Haskell"
    putStrLn "El arbol esta vacio. Selecciona '1' para empezar."
    
    -- Start the main loop with 'Nothing', representing an empty tree.
    mainLoop Nothing