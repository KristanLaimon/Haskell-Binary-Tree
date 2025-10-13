module MyModule (consoleLog) where

-- | Prints a string to the console, followed by a newline.
--   This is an alias for `putStrLn`.
consoleLog:: String -> IO()
consoleLog = putStrLn
