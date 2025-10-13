import System.IO

main = appendFile "tabla.txt" (show [(x, x*x) | x <- [1..10]])

-- main_2 = do 
--   arch <- openFile "archivo.txt" ReadMode
--   contents <- hGetContents arch 
--   putStr contents
--   hClose arch

-- Funciones sobre contenedores 


