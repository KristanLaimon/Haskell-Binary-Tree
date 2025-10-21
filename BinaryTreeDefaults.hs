module BinaryTreeDefaults (exampleRootNode) where
import BinaryTree(Node(..), nodeCreateFromList)

-- | A default root node to be used for examples or testing. From 1 to 10
exampleRootNode :: Node
exampleRootNode = nodeCreateFromList [5,4,6,3,7,2,8,1,9,0,10]