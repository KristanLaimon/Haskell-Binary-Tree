{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module BinaryTree (
    -- Constructors
    Node(..), 
    nodeCreateSimpleWithValue,
    nodeCreateEmpty,
    nodeCreateFromList,

    --Insertion
    nodeInsertInto, 
    nodeInsertIntoWithList, 

    --Searching
    nodeSearchValueFrom, 

    --Travelling
    nodeToString
) where

-- | Represents a single node in a binary search tree.
-- Each node contains an integer value and may have left and right children.
data Node = Node {
  value     :: Integer,    -- ^ The integer value stored in the node.
  leftNode  :: Maybe Node, -- ^ The left child node, which is smaller than the current node's value or 'Nothing'
  rightNode :: Maybe Node  -- ^ The right child node, which is larger than the current node's value or 'Nothing'
}

-- | Creates a single root node with a specified value and no children.
nodeCreateSimpleWithValue :: Integer -> Node
nodeCreateSimpleWithValue value = Node{value=value, leftNode=Nothing, rightNode=Nothing}

-- | Creates a default node with value 0.
-- Note: This represents a single-node tree, not a truly empty tree.
nodeCreateEmpty :: Node
nodeCreateEmpty = nodeCreateSimpleWithValue 0

-- | Creates a binary search tree from a list of integers.
-- The first element of the list becomes the root, and the rest are inserted sequentially.
-- An empty list will produce a single-node tree with the value 0.
nodeCreateFromList :: [Integer] -> Node
nodeCreateFromList [] = nodeCreateEmpty
nodeCreateFromList (x:xs) = 
    let root = nodeCreateSimpleWithValue x 
        in foldl nodeInsertInto root xs


-- ================ 1. Insertar =======================
-- Helper to handle Maybe Nodes (null | Node equivalent)
nodeInsertIntoMaybe :: Maybe Node -> Integer -> Maybe Node
nodeInsertIntoMaybe Nothing newValue = Just (Node {value = newValue, leftNode = Nothing, rightNode = Nothing})
nodeInsertIntoMaybe (Just node) newValue = Just (nodeInsertInto node newValue)

-- | Insert a new node into existing root node (or any node actually)
--
-- @param 'Node' node | Node to insert into
--
-- @param 'Integer' newValue | Integer value to insert through whole tree node from 'node' as root
--
-- @returns 'Node' | A new node containing the added node as son through all the tree node
nodeInsertInto :: Node -> Integer -> Node
nodeInsertInto node newValue
    | newValue < value node = node {leftNode = nodeInsertIntoMaybe (leftNode node) newValue}
    | newValue > value node = node {rightNode = nodeInsertIntoMaybe (rightNode node) newValue}
    | otherwise = node -- The value is equal. We do nothing (don't add duplicates).

nodeInsertIntoWithList :: Node -> [Integer] -> Node
nodeInsertIntoWithList node listToAdd = foldl nodeInsertInto node listToAdd


-- ============ 2. Buscar ==============
-- Helper to handle Maybe Nodes (null | Node equivalent)
nodeSearchValueFromMaybe :: Maybe Node -> Integer -> Maybe Node
nodeSearchValueFromMaybe Nothing valueToSearch = Nothing
nodeSearchValueFromMaybe (Just node) valueToSearch = nodeSearchValueFrom node valueToSearch

-- | Search a node using binary search (Preorder)
--
-- @param 'Node' node | Node to start searching from
--
-- @param 'Integer' valueToSearch | Value to search through the whole node tree starting as root from 'node' param
--
-- @returns 'Maybe Node' node | A proper 'Just' node if found, 'Nothing' otherwise
nodeSearchValueFrom :: Node -> Integer -> Maybe Node
nodeSearchValueFrom myNode valueToSearch
    | value myNode == valueToSearch = Just myNode
    | valueToSearch < value myNode = nodeSearchValueFromMaybe (leftNode myNode) valueToSearch
    | otherwise = nodeSearchValueFromMaybe (rightNode myNode) valueToSearch


-- ============== 3. Recorrer (Preorder) ================
-- Helper to handle Maybe Nodes (null | Node equivalent)
nodeToStringMaybe :: Maybe Node -> String -> String -> String
nodeToStringMaybe Nothing indent prefix = indent ++ prefix ++ "#\n"
nodeToStringMaybe (Just node) indent prefix =
    indent ++ prefix ++ show (value node) ++ "\n" ++
    let newIndent = indent ++ "  " in
        nodeToStringMaybe (leftNode node) newIndent "L: " ++
        nodeToStringMaybe (rightNode node) newIndent "R: "

-- | Convert whole tree node (starting from a root node) to a representable and easy representation string
-- 
-- @param 'Node' node | Root node to start the string representation return
--
-- @returns 'String' outputStr | Full string representation for 'node' root node param!
nodeToString :: Node -> String
nodeToString node =
    show (value node) ++ "\n" ++
    nodeToStringMaybe (leftNode node) "  " "L: " ++
    nodeToStringMaybe (rightNode node) "  " "R: "

-- ================ 4. Sumar (Preorder) =======================
