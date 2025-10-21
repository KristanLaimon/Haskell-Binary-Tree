module BinaryTreeDefaults (exampleRootNode) where
import BinaryTree(Node(..), nodeInsertIntoWithList)

-- | A default root node to be used for examples or testing. From 1 to 10
exampleRootNode :: Node
exampleRootNode = Node {value=5, leftNode=Nothing, rightNode=Nothing}