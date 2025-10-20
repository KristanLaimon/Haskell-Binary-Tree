import Binarytree(Node(..), nodeInsertInto, nodeInsertIntoMaybe, nodeToString )

main :: IO()
main = do
  let rootNode = Node{value=1, leftNode=Nothing, rightNode=Nothing}
  let rootNode = nodeInsertInto rootNode 3

  putStrLn $ nodeToString rootNode
