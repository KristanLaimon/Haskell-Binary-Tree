import BinaryTree(Node(..), nodeInsertInto, nodeToString, nodeSearchValueFrom, nodeCreateSimpleWithValue )
import BinaryTreeDefaults (exampleRootNode)

main :: IO()
main = do
  let rootNode :: Node = nodeCreateSimpleWithValue 11
  let rootNodeWith3 :: Node = nodeInsertInto rootNode 3

  let foundNode :: Maybe Node = nodeSearchValueFrom rootNodeWith3 3
  case foundNode of
    Just node -> do
      putStrLn "Node Found!"
      putStrLn $ nodeToString node
    Nothing -> do
      putStrLn "Node not found..."

  putStrLn "======================"

  putStrLn $ nodeToString exampleRootNode


