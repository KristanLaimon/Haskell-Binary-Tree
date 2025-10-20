import System.IO

lectorDatos = do 
  contenedor <- openFile "pokemones.txt" ReadMode
  contenido <- hGetContents contenedor
  putStr contenido
  hClose contenedor

main :: IO ()
main = lectorDatos
