module Binarytree (Node(..), nodeInsertInto, nodeInsertIntoMaybe, nodeToString) where

data Node = Node {
  value     :: Integer,
  leftNode  :: Maybe Node, 
  rightNode :: Maybe Node
} deriving (Show) 


-- == Insert() | 1. Insertar ==
nodeInsertIntoMaybe :: Maybe Node -> Integer -> Maybe Node
nodeInsertIntoMaybe Nothing newValue =
    Just (Node {value = newValue, leftNode = Nothing, rightNode = Nothing})
nodeInsertIntoMaybe (Just node) newValue =
    Just (nodeInsertInto node newValue)

nodeInsertInto :: Node -> Integer -> Node
nodeInsertInto node newValue
    | newValue < value node =
        node {leftNode = nodeInsertIntoMaybe (leftNode node) newValue}
    | newValue > value node =
        node {rightNode = nodeInsertIntoMaybe (rightNode node) newValue}
    | otherwise = node -- The value is equal. We do nothing (don't add duplicates).

-- === ToString() | 2. Recorrer (Preorden) Root => arbol derecho => arbol derecho === --
nodeToString :: Node -> String
nodeToString node =
    show (value node) ++ "\n" ++
    prettyPrintMaybe (leftNode node) "  " "L: " ++
    prettyPrintMaybe (rightNode node) "  " "R: "

prettyPrintMaybe :: Maybe Node -> String -> String -> String
prettyPrintMaybe Nothing indent prefix = indent ++ prefix ++ "#\n"
prettyPrintMaybe (Just node) indent prefix =
    indent ++ prefix ++ show (value node) ++ "\n" ++ 
    let newIndent = indent ++ "  " in
        prettyPrintMaybe (leftNode node) newIndent "L: " ++
        prettyPrintMaybe (rightNode node) newIndent "R: "