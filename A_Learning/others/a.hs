-- Listas con calificaciones | DATA
alumno1:: [Int]
alumno1 = [70,71,69,89]

alumno2:: [Int]
alumno2 = [90, 85, 95, 100]

alumno3:: [Int]
alumno3 = [95, 96, 97, 98]

-- Ejercicio 1
firstList:: [Int]
firstList = alumno1 ++ alumno2 ++ alumno3

-- Ejercicio 2
secondList:: [[Int]]
secondList = [alumno1, alumno2, alumno3]

secondListDifferent :: [[Int]]
secondListDifferent = [alumno1, alumno2, alumno3]

main :: IO ()
main = putStrLn "Hola Mundo"

suma:: Integer -> Integer
suma 1 = 1
suma n = n + suma (n-1)

factorial:: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

max3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z

hip :: Float -> Float -> Float
hip a b = sqrt (a**2 + b**2)

multiplo :: Int -> [Int] -> [Int]
multiplo n [] = []
multiplo n (x:xs) = [x|x<-xs, mod x n == 0 ]

divisible :: Int -> [Int] -> [String]
divisible n list = [if mod x n == 0 then show x ++ "-D" else show x ++ "-ND" | x <- [0.. length list - 1]]


data Rectangulo = Rectangulo  Float Float