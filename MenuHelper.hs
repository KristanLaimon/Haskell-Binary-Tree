-- File: MenuHelper.hs
module MenuHelper (displayMenuAndGetChoice) where

import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

-- | Displays a menu of options, prompts the user for a choice,
-- | and returns the valid selected option number as an Integer.
-- | It will re-prompt if the input is not a valid number or
-- | is outside the range of options.
displayMenuAndGetChoice :: [String] -> IO Integer
displayMenuAndGetChoice options = do
    -- Display all menu options
    putStrLn "\n==================== MENU ===================="
    mapM_ putStrLn options
    putStrLn "=============================================="
    
    -- Start the loop to get a valid choice
    getChoiceLoop (length options)

-- | Internal helper function to recursively ask for input
-- | until a valid choice is made.
getChoiceLoop :: Int -> IO Integer
getChoiceLoop numOptions = do
    putStr "Select an option (1-?): "
    hFlush stdout  -- Ensure the prompt appears before getLine
    
    input <- getLine
    
    -- Safely try to parse the input string to an Int
    let maybeInt = readMaybe input :: Maybe Int
    
    case maybeInt of
        -- Case 1: Input was not a number
        Nothing -> do
            putStrLn "\n[Error] Invalid input. Please enter a number."
            getChoiceLoop numOptions -- Recurse
            
        -- Case 2: Input was a number
        Just val -> do
            -- Check if the number is within the valid option range
            if val >= 1 && val <= numOptions
                then return (fromIntegral val) -- Valid: return the value
                else do
                    putStrLn $ "\n[Error] Invalid option. Please select a number between 1 and " ++ show numOptions ++ "."
                    getChoiceLoop numOptions -- Recurse