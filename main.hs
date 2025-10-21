import Binarytree(Node(..), nodeInsertInto, nodeToString, nodeSearchValueFrom )

main :: IO()
main = do
  let rootNode = Node{value=1, leftNode=Nothing, rightNode=Nothing}
  let rootNodeWith3 = nodeInsertInto rootNode 3

  let foundNode = nodeSearchValueFrom rootNodeWith3 3
  case foundNode of
    Just node -> do
      putStrLn "Node Found!"
      putStrLn $ nodeToString node
    Nothing -> do
      putStrLn "Node not found..."


