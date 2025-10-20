import Data.Char (toUpper)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- -------------------------------------------
-- Definicion de funciones basicas en Haskell 
-- -------------------------------------------

-- funcion que devuelve el mayor de 3 numeros enteros
-- max3 ::Int -> Int -> Int -> Int
-- sin patrones indica que x es el primer parametro, y
-- el segundo y z el tercero y no tiene en cuenta el tipo de dato
max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

-- funcion que calcula la hipotenusa de un triangulo rectangulo
-- haciendo uso de una funcion auxiliar cuadrado
hipotenusa:: Float -> Float -> Float
hipotenusa a b = sqrt (cuadrado a + cuadrado b)

-- funcion que calcula el cuadrado de un numero
-- usando tipado generico Num a => a -> a es decir acepta cualquier tipo numerico
cuadrado :: Num a => a -> a
cuadrado a = a * a

-- funcion que dado un divisor y una lista de enteros
-- devuelve una lista de strings indicando "Si" o "No"
-- si cada elemento de la lista es divisible por el divisor
-- Nota: usa listas por comprension y operador mod
divisible :: Int -> [Int] -> [String]
divisible div list = [if x `mod` div == 0 then show x ++ "-D" else show x ++ "-ND" | x <- list]

-- funcion factorial usando guardas
-- guardas: condiciones que se evaluan en orden
fact n | n == 0 = 1
       | n > 0 = n * fact (n - 1)
       | otherwise = error "Factorial no definido para numeros negativos"

-- funcion que calcula el maximo comun divisor (mcd) usando el algoritmo de Euclides
mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b) -- mod a b tambien es valido

sign x | x < 0 = -1
       | x == 0 = 0
       | x > 0 = 1

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- ---------------------------------------------------
-- Definicion de tipos de datos data (personalizados)
-- ---------------------------------------------------

-- Ajuste de patrones: consiste en una especificacion de pautas que
-- deben ser seguidas por los datos, los caules pueden ser 
-- desconstruidos permitiendonos acceder a sus componentes
dia::Int -> String
dia 1 = "lunes"
dia 2 = "martes"
dia 3 = "miercoles"
dia 4 = "jueves"
dia 5 = "viernes"
dia 6 = "sabado"
dia 7 = "domingo"
dia x = "no se"

data Cliente = Cliente {
nombreCli::String,
edadCli::Int,
saldoCli::Float
} deriving(Show, Eq)
-- tipo de dato DiaSemana y funcion humor que devuelve
-- un string segun el dia de la semana
{-
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 

humor::DiaSemana -> String
humor Lunes = "Triste"
humor Viernes = "Feliz"
-}
-- definiendo derivacion de DiaSemana con Eq para poder comparar
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving(Eq)

-- funcion humor que devuelve un string segun el dia de la semana con comparacion
humor::DiaSemana -> String
humor dia
    | dia == Lunes = "Triste"
    | dia == Viernes = "Feliz"
    | otherwise = "Serio"

-- tipo de dato Figura y funcion area que calcula el area
{-
data Rectangulo = Rectangulo Float Float
area::Rectangulo -> Float
area (Rectangulo ancho alto) = ancho * alto

data Circulo = Circulo Float
area:: Circulo -> Float
area (Circulo radio) = 3.1416 * radio * radio
-}

-- uniendo ambos en un solo tipo de dato Figura
data Figura = Rectangulo Float Float | Circulo Float

-- funcion area que calcula el area de una Figura usando patrones de construccion
area::Figura->Float
area(Rectangulo ancho alto) = ancho * alto
area(Circulo radio) = 3.1416 * radio * radio
---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- -------------------------------------------
-- Entrada y salida basica en Haskell
-- -------------------------------------------

{- Entrada y salida

Diferencia entre putStr, putStrLn y print:
- putStr: imprime una cadena sin salto de linea al final
- putStrLn: imprime una cadena con salto de linea al final
- print: convierte cualquier valor en cadena y lo imprime con salto de linea al final
-- show: convierte un valor en una representacion de cadena (usado internamente por print)

GetLine: lee una linea de entrada del usuario como cadena
main :: IO () permite definir la funcion principal del programa
do: permite encadenar varias acciones de entrada/salida de forma secuencial
$: operador que aplica una funcion a un valor, evitando paréntesis adicionales
-}

{-
main = do
    putStrLn "Hola, ¿cómo te llamas?"
    nombre <- getLine
    putStrLn ("Hey " ++ nombre ++ "!")

-}

{-
Ejemplo que convierte el nombre y apellido a mayusculas
import Data.Char
main = do
    putStrLn "Cual es tu nombre?"
    nombre <- getLine
    putStrLn "Cual es tu appellido?"
    apellido <- getLine
    let mayusNombre = map toUpper nombre
        mayusApellido = map toUpper apellido
    putStrLn $ "Hola " ++ mayusNombre ++ " " ++ mayusApellido ++ "!"
-}
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- -------------------------------------------
-- Trabajando con listas en Haskell
-- -------------------------------------------

main = do
-- lista de datos en haskell  
-- listas se definen entre corchetes y los elementos separados por comas
-- pueden ser de cualquier tipo pero todos deben ser del mismo tipo
    let datos = [3, 5, 2, 7, 8]
    print datos

-- Concatenacion de listas
    let lista1 = [1, 2, 3]
        lista2 = [4, 5, 6] -- se puede omitir el let en la segunda linea debido a la indentacion
        listaConcatenada = lista1 ++ lista2
    putStr $ "Lista concatenada: " ++ show listaConcatenada
    print listaConcatenada -- Igual que putStrLn con show

-- Cadenas son Listas
    let cadena1 = "Hola, "
        cadena2 = "Mundo!"
        cadenaConcatenada = cadena1 ++ cadena2
    putStrLn $ "Cadena concatenada: " ++ cadenaConcatenada

-- "Hola" es lo mismo que ['H','o','l','a']
    let holaLista = ['H','o','l','a']
    putStrLn $ "Hola como lista de caracteres: " ++ holaLista

-- Concatenar al inicio de una lista mediante el operador :
    let listaOriginal = [2, 3, 4]
        listaConUnoAlInicio = 1 : listaOriginal --o tambien 1 ++ listaOriginal
    putStrLn $ "Lista con 1 al inicio: " ++ show listaConUnoAlInicio

-- tambien podemos concatenar al inicio de una cadena un char
-- no podemos concatenar una cadena al inicio de otra con : debido a que
-- : solo concatena un elemento al inicio de una lista
-- para cadenas usamos ++
-- ya que las cadenas son listas de caracteres
    let cadenaOriginal = "mundo!"
        cadenaConHolaAlInicio = 'h': 'o': 'l' : 'a' : cadenaOriginal
    putStrLn $ "Cadena con 'Hola, ' al inicio: " ++ cadenaConHolaAlInicio

-- listas vacias
    let listaVacia = []
    putStrLn $ "Lista vacia: " ++ listaVacia -- imprime nada

-- [] es usado para generar listas vacias o tambien para generar una lista a partir de un solo elemento
    let unElemento = 'S'
        listaConUnElemento =  unElemento : [] -- o tambien [unElemento]
    putStrLn $ "Lista con un elemento: " ++ listaConUnElemento

-- obtener un elemento de una lista por su indice
    let listaIndices = [10, 20, 30, 40, 50]
        elementoEnIndice2 = listaIndices !! 2 -- indices empiezan en 0
    putStrLn $ "Elemento en indice 2: " ++ show elementoEnIndice2

-- listas de listas
    let matriz = [[1,2,3], [4,5,6], [7,8,9]]
        fila1 = matriz !! 0 -- o usar head matriz
        elementoFila2Columna3 = matriz !! 1 !! 2
    putStrLn $ "Fila 1 de la matriz: " ++ show fila1
    putStrLn $ "Elemento en fila 2, columna 3: " ++ show elementoFila2Columna3

-- ejercicios con listas con base a lo anterior
    let calificaciones1 = [80, 90, 75, 85]
        calificaciones2 = [88, 92, 79, 95]
        calificaciones3 = [70, 60, 50, 40]
    -- lista con todas las calificaciones
        todasCalificaciones = calificaciones1 ++ calificaciones2 ++ calificaciones3
    putStrLn $ "Lista con todas las calificaciones" ++ show todasCalificaciones

    -- incorporar las tres listas como elementos de una lista que los contenga
    let listasCalificaciones = [calificaciones1, calificaciones2, calificaciones3]
    -- tambien se puede mediante calificaciones1:calificaciones2:calificaciones3:[] (opcional [])
    -- debido a que calificaciones1, calificaciones2 y calificaciones3 son listas
    putStrLn $ "Lista de listas con todas las calificaciones" ++ show listasCalificaciones

    -- mostrar las calificaciones del alumno 2 (segunda lista)
    let calificacionesAlumno2 = listasCalificaciones !! 1
    putStrLn $ "Calificaciones del alumno 2: " ++ show calificacionesAlumno2

-- funciones utiles para listas
-- head: obtiene el primer elemento de una lista
-- tail: obtiene todos los elementos de una lista excepto el primero
-- last: obtiene el ultimo elemento de una lista
-- init: obtiene todos los elementos de una lista excepto el ultimo
-- length: obtiene la longitud de una lista
-- null: verifica si una lista es vacia
-- reverse: invierte una lista
-- take n lista: obtiene los primeros n elementos de una lista
-- drop n lista: elimina los primeros n elementos de una lista
-- maximum: obtiene el maximo elemento de una lista
-- minimum: obtiene el minimo elemento de una lista
-- sum: suma todos los elementos de una lista
-- product: multiplica todos los elementos de una lista
-- elem x lista: verifica si un elemento x esta en la lista

-- Usando las funciones head, tail, last e init
    let listaEjemplo = [10, 20, 30, 40, 50]
        primerElemento = head listaEjemplo -- tambien listaEjemplo !! 0
        restoElementos = tail listaEjemplo -- todos menos el primero
        ultimoElemento = last listaEjemplo -- ultimo elemento tambien se puede usar listaEjemplo !! (length listaEjemplo - 1)
        todosMenosUltimo = init listaEjemplo -- todos menos el ultimo
    putStrLn $ "Primer elemento: " ++ show primerElemento
    putStrLn $ "Resto de elementos: " ++ show restoElementos
    putStrLn $ "Ultimo elemento: " ++ show ultimoElemento
    putStrLn $ "Todos menos el ultimo: " ++ show todosMenosUltimo

-- Usando length, null, reverse
    let longitudLista = length listaEjemplo
        esListaVacia = null listaEjemplo
        listaInvertida = reverse listaEjemplo
    putStrLn $ "Longitud de la lista: " ++ show longitudLista
    putStrLn $ "¿La lista es vacia?: " ++ show esListaVacia
    putStrLn $ "Lista invertida: " ++ show listaInvertida

-- Usando take, drop, maximum, minimum, sum, product, elem
    let primeros3 = take 3 listaEjemplo
        sinPrimeros3 = drop 3 listaEjemplo
        maximoElemento = maximum listaEjemplo
        minimoElemento = minimum listaEjemplo
        sumaElementos = sum listaEjemplo
        productoElementos = product listaEjemplo
        contiene30 =  elem 30 listaEjemplo -- tambien se puede usar 30 `elem` listaEjemplo
    putStrLn $ "Primeros 3 elementos: " ++ show primeros3
    putStrLn $ "Lista sin los primeros 3 elementos: " ++ show sinPrimeros3
    putStrLn $ "Maximo elemento: " ++ show maximoElemento
    putStrLn $ "Minimo elemento: " ++ show minimoElemento
    putStrLn $ "Suma de elementos: " ++ show sumaElementos
    putStrLn $ "Producto de elementos: " ++ show productoElementos
    putStrLn $ "¿La lista contiene 30?: " ++ show contiene30

-- ejercicios 2 usando las calificaciones anteriores
    -- mostrar las calificaciones en orden inverso del tercer alumno
    let calificacionesAlumno3 = listasCalificaciones !! 2
        calificacionesAlumno3Invertidas = reverse calificacionesAlumno3
    putStrLn $ "Calificaciones del alumno 3 en orden inverso: " ++ show calificacionesAlumno3Invertidas

    -- crear una lista con todas las calificaciones de los primeros dos alumnos
    let calificacionesPrimerosDosAlumnos = take 2 listasCalificaciones
        todasCalificacionesPrimerosDos = concat calificacionesPrimerosDosAlumnos
    putStrLn $ "Todas las calificaciones de los primeros dos alumnos: " ++ show todasCalificacionesPrimerosDos
    -- concat: une una lista de listas en una sola lista
    -- tambien se puede:
    -- let todasCalificacionesPrimerosDos = (listasCalificaciones !! 0) ++ (listasCalificaciones !! 1)
    
    -- Mostrar la calificacion maxima del primer alumno
    let calificacionesAlumno1 = head listasCalificaciones
        maxCalificacionAlumno1 = maximum calificacionesAlumno1
    putStrLn $ "Calificacion maxima del alumno 1: " ++ show maxCalificacionAlumno1

    -- preguntar si un alumno obtuvo una calificacion de 70
    let obtuvo70Alumno1 = 70 `elem` head listasCalificaciones
    putStrLn $ "¿El alumno 1 obtuvo una calificacion de 70?: " ++ show obtuvo70Alumno1
    let obtuvo70Alumno2 = 70 `elem` (listasCalificaciones !! 1)
    putStrLn $ "¿El alumno 2 obtuvo una calificacion de 70?: " ++ show obtuvo70Alumno2
    let obtuvo70Alumno3 = 70 `elem` (listasCalificaciones !! 2)
    putStrLn $ "¿El alumno 3 obtuvo una calificacion de 70?: " ++ show obtuvo70Alumno3 

    -- Obtener el promedio de calificaciones del segundo alumno
    let calificacionesAlumno2List = listasCalificaciones !! 1
        promedioAlumno2 = fromIntegral (sum calificacionesAlumno2List) / fromIntegral (length calificacionesAlumno2List)
    putStrLn $ "Promedio de calificaciones del alumno 2: " ++ show promedioAlumno2
    -- fromIntegral: convierte un entero a un numero de punto flotante

-- Rangos
    let rango1a10 = [1..10] -- lista de numeros del 1 al 10
        rangoAtoZ = ['A'..'Z'] -- lista de caracteres de la A a la Z
    putStrLn $ "Rango de 1 a 10: " ++ show rango1a10
    putStrLn $ "Rango de A a Z: " ++ show rangoAtoZ

-- listas por comprension (intencionadas)
-- estas listas se generan a partir de una expresion y una o mas condiciones
-- se basan en la notacion matematica de conjuntos
-- ejemplo: {x^2 | x ∈ [1..10]}
    let cuadrados = [x * x | x <- [1..10]] -- lista de cuadrados de 1 a 10
    putStrLn $ "Cuadrados de 1 a 10: " ++ show cuadrados

-- ejemplo con condicion: {x | x ∈ [1..20], x es par}
-- usando la condicion even que devuelve True si un numero es par
    let pares = [x | x <- [1..20], even x] -- lista de numeros pares del 1 al 20
    putStrLn $ "Numeros pares del 1 al 20: " ++ show pares

-- ejemplo con condicion: {x | x ∈ [1..30], x es multiplo de 3}
    let multiplosDe3 = [x | x <- [1..30], x `mod` 3 == 0] -- lista de multiplos de 3 del 1 al 30
    putStrLn $ "Multiplos de 3 del 1 al 30: " ++ show multiplosDe3

-- es posible usar multiples generadores y condiciones
-- ejemplo: {x + y | x ∈ [1..5], y ∈ [6..10], x + y es par}
    let sumaPares = [x + y | x <- [1..5], y <- [6..10], even (x + y)] -- lista de sumas pares
    putStrLn $ "Suma de x e y donde x -> [1..5], y -> [6..10] y x + y es par: " ++ show sumaPares

-- Ejercicio obtener los numeros del 1 al 50 que son multiplos de 4
    let multiplosde4 = [x | x <- [1..50], mod x 4 == 0]
    putStrLn $ "Multiplos de 4 del 1 al 50: " ++ show multiplosde4

-- Ejercicio escribir una funcion "multiplos" que reciba un valor n y una lista
-- de enteros, y nos devuelva una sublista de los elementos que son múltiplos de n
    let multiplo:: Int -> [Int] -> [Int]
        multiplo n lista = [x | x <- lista, mod x n == 0]
        multiplo7 = multiplo 7 [1..100]
    putStrLn $ "Multiplos de 7 del 1 al 100 usando la funcion multiplo: " ++ show multiplo7

-- Ejemplo Obtener las posiciones de un caracter dentro de una cadena
    let carrera = "Sistemas"
        posicionesLetraS = [x + 1 | x <-[0 .. length carrera - 1], carrera!!x == 's']
    putStrLn $ "Posiciones del caracter s en la cadena Sistemas: " ++ show posicionesLetraS

-- Ejercicio en el ejemplo anterior que incluya tanto mayúsculas como minúsculas
    let posicionesLetraS = [x + 1 | x <-[0 .. length carrera - 1], carrera!!x == 's' || carrera!!x == 'S']
    putStrLn $ "Posiciones del caracter s y S en la cadena Sistemas: " ++ show posicionesLetraS

-- Ejercicio escribir la funcion posCar que reciba como parametros la cadena y el caracter a buscar dentro de la cadena
    let posCar:: String -> Char -> [Int]
        posCar cadena car = [x + 1 | x <-[0 .. length cadena - 1], cadena!!x == car]
    putStrLn $ "Posiciones del caracter s usando posCar: " ++ show (posCar "Sistemas" 's')

-- Ejemplo Obtener todas las subcadenas de un tamaño dado dentro de una cadena
    let subcadenasCarrera = [take 3 (drop x carrera) | x <- [0..length carrera - 3]] 
    putStrLn $ "Todas las subcadenas de tamaño 3 de la cadena Sistemas: " ++ show subcadenasCarrera

-- Uso de if-then-else en listas intensionales
    let lista = [80, 60, 90, 50]
    let listaAprobado = [if x > 70 then "Aprobado" else "NA" | x <- lista]
    putStrLn $ "Lista de Aprobados: " ++ show listaAprobado

    let listaAprobado = [if x > 70 then show x else "NA" | x <- lista]
    putStrLn $ "Lista de Aprobados con Calificacion: " ++ show listaAprobado

-- Ejercicio Escribir la funcion divisible
    let lista = [1..10]
    let valor = 2
    let listaDivisible = divisible valor lista
    putStrLn $ "Lista de Numeros del 1 al 10 divisibles entre 2: " ++ show listaDivisible

-- Ejemplo con Listas de Listas: promedio de las calificaciones por alumno
    let todasCalif = [[90, 80, 85, 78], [96, 87, 79, 100], [88, 89, 97, 95]]
    let promedioCalif = [fromIntegral(sum alumCalif)/fromIntegral(length alumCalif) | alumCalif <- todasCalif]
    putStrLn $ "Promedio de calificaciones de cada alumno: " ++ show promedioCalif

-- Ejercicio Obtener una lista con la calificacion maxima de cada alumno
    let califMasAlta = [maximum alumCalif | alumCalif <- todasCalif]
    putStrLn $ "Calificacion mas alta de cada alumno: " ++ show califMasAlta

-- Ejercicio Obtener una lista de listas con la calificacion minima y maxima de cada alumno
    let califMinyMax = [[maximum alumCalif, minimum alumCalif] | alumCalif <- todasCalif]
    putStrLn $ "Calificacion minima y maxima de cada alumno: " ++ show califMinyMax

-- Ejemplo generacion de una lista de forma anidada calificaciones mayores o iguales a 90 de cada alumno
    let califMayores90 = [[calif | calif <- alumCalif, calif>=90]|alumCalif <- todasCalif]
    putStrLn $ "Calificaciones mayores o iguales a 90 de cada alumno: " ++ show califMayores90

-- Ejercicio convertir a mayusculas todos los caracteres de la siguiente lista.
    let carreras = ["sistemas", "civil", "contabilidad", "electromecanica"]
        carrerasMayus = [map toUpper c | c <- carreras]
    putStrLn $ "Carreras en minusculas: " ++ show carreras
    putStrLn $ "Carreras en mayusculas: " ++ show carrerasMayus

-- -------------------------------------------
-- Trabajando con tuplas en Haskell
-- -------------------------------------------

-- Son utilizadas cuando sabemos cuantos valores tienen que ser utilizados
-- No tienen que ser del mismo tipo (homogeneas)
-- Se definen con parentesis y sus valores se separan con comas
-- Ejemplos
    let alumno1 = ("Jose Perez", 100)
    putStrLn $ "Alumno 1: " ++ show alumno1
    let alumno2 = ("Maria Sanchez", 80)
    putStrLn $ "Alumno 2: " ++ show alumno2
    let alumno3 = ("Juan Cota", 90)
    putStrLn $ "Alumno 3: " ++ show alumno3

    let datos = alumno1:alumno2:alumno3:[] --[alumno1, alumno2, alumno3]
    putStrLn $ "Alumnos: " ++ show datos

-- funciones para tuplas de dos elementos
-- fst: devuelve el primer componente de una tupla
-- snd: devuelve el segundo elemento
    let nombreA1 = fst alumno1
    putStrLn $ "Nombre Alumno 1: " ++ show nombreA1
    let califA1 = snd alumno1
    putStrLn $ "Calificacion Alumno 1: " ++ show califA1

-- Ejercicio obtener una lista con las calificaciones de los tres alumnos
    let listaCalif = [snd alumno1, snd alumno2, snd alumno3]
    putStrLn $ "Calificaciones de los 3 alumnos: " ++ show listaCalif

-- Obtener el promedio de las tres calificaciones
    let promedioCalif = fromIntegral (sum listaCalif) / fromIntegral (length listaCalif)
    putStrLn $ "Promedio 3 calificaciones: " ++ show promedioCalif

-- Ejemplo tuplas de 3 elementos
    let alumno1 = ("Jose Perez", "Matematicas", 100)
    putStrLn $ "Alumno 1 con 3 elementos: " ++ show alumno1

-- Las funciones para tuplas de 3 o mas elementos se deben crear
-- En el ejemplo de 3 tuplas se deben crear un metodo para acceder a cada valor
    let nombre::(String, String, Integer) -> String
        nombre(nombre, materia, calif) = nombre
    putStrLn $ "Nombre alumno1: " ++ nombre alumno1 

    let materia::(String, String, Integer) -> String
        materia(nombre, materia, calif) = materia
    putStrLn $ "Materia alumno1: " ++ materia alumno1

    let calif::(String, String, Integer) -> Integer
        calif(nombre, materia, calif) = calif
    putStrLn $ "calif alumno1: " ++ show (calif alumno1)

-- Ejercicio escribir una funcion datosCalif
    let datosCalif::(String, String, Integer) -> String
        datosCalif(nombre, materia, calif) = "nombre:" ++ nombre ++ ",materia:" ++  materia ++ ",calif:" ++ show calif
    putStrLn $ "Datos alumno1: " ++ datosCalif alumno1

-- -------------------------------------------
-- Trabajando con registros en Haskell
-- -------------------------------------------
-- Los registros automatizan la creacion de funciones para acceder a los elementos
-- Se definen mediante {}
{-
data Cliente = Cliente {
nombre::String,
edad::Int,
saldo::Float
}
-}
    let cli1 = Cliente {nombreCli = "Jose", edadCli = 20, saldoCli = 1000.0}
    putStrLn $ "Nombre Cliente1: " ++ nombreCli cli1
    putStrLn $ "Edad Cliente1: " ++ show (edadCli cli1)
    putStrLn $ "Saldo Cliente1: " ++ show (saldoCli cli1)

    let cli2 = Cliente {nombreCli = "Martha", edadCli = 22, saldoCli = 2000.0}
    let cli3 = Cliente {nombreCli = "Juan", edadCli = 20, saldoCli = 1500.0}
    let clientes = [cli1, cli2, cli3]
    putStrLn $ "Clientes: " ++ show clientes

    let cliente1 = clientes!!0
    putStrLn $ "Cliente1: " ++ show cliente1

    let nombreCli1 = nombreCli(clientes!!0)
    putStrLn $ "Nombre Cliente1: " ++ nombreCli1

-- Ejercicio funcion sumaSaldo que reciba una lista de Clientes
-- y devuleva la suma del saldo de todos los clientes de la lista
    let sumaSaldo :: [Cliente] -> Float
        sumaSaldo lista = sum [saldoCli x | x <- lista]
    putStrLn $ "Suma del Saldo: " ++ show (sumaSaldo clientes)

-- Ejercicio funcion saldoPromedio que devuelve el saldo promedio
-- de una lista de clientes
    let saldoPromedio :: [Cliente] -> Float
        saldoPromedio lista = sumaSaldo clientes / fromIntegral (length lista)
    putStrLn $ "Promedio del Saldo: " ++ show (saldoPromedio clientes)

-- Ejercicio funcion clienteMax que reciba una lista de clientes, y devuelve
-- los datos del cliente con saldo mayor
    let clienteMax :: [Cliente] -> Cliente
        clienteMax lista = maximumBy (comparing saldoCli) clientes 
    putStrLn $ "Cliente con Mayor Saldo: " ++ show (clienteMax clientes)

-- -------------------------------------------
-- Trabajando con recursividad en Haskell
-- -------------------------------------------

-- Es la forma de definir una funcion en la que dicha funcion se utiliza en su propia definicion

-- Ejemplo funcion para calcular el factorial de un numero
    let factorial:: Integer -> Integer
        factorial 1 = 1
        factorial n = n * factorial (n - 1)
    putStrLn $ "Factorial de 4: " ++ show (factorial 4)

-- Ejercicio programar una funcion recursiva para sumar los numeros del 1 al n
    let suma :: Integer -> Integer 
        suma 1 = 1
        suma n = n + suma (n - 1)
    putStrLn $ "Sumar los numeros del 1 al 4: " ++ show (suma 4)

-- Ejercicio escribir una funcion que sume los cuadrados de los numeros del 1 al n
    let sumaCuadrado :: Integer -> Integer 
        sumaCuadrado 1 = 1
        sumaCuadrado n = cuadrado n + sumaCuadrado (n - 1)
    putStrLn $ "Sumar los cuadrados de los numeros del 1 al 4: " ++ show (sumaCuadrado 4)

-- Ejercicio suma los cuadrados de los numeros pares del 1 al n
    let sumaCuadradoPares :: Integer -> Integer 
        sumaCuadradoPares 1 = 0
        sumaCuadradoPares n = if mod n 2 == 0 then cuadrado n + sumaCuadradoPares (n - 1) else sumaCuadradoPares (n - 1)
    putStrLn $ "Sumar los cuadrados de los numeros pares del 1 al 4: " ++ show (sumaCuadradoPares 4)

-- -------------------------------------------
-- Trabajando con archivos en Haskell
-- -------------------------------------------
-- Se usa la biblioteca System.IO
-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath = String
-- data IOMode = ReadMode
--              |WriteMode
--              |AppendMode
--              |ReadWriteMode
-- IO Handle es un contenedor

-- hgetContents :: Handle -> IO String
-- hClose :: Handle -> IO
{-
import System.IO
main = appendFile "tabla.text" (show [(x, x*x)|x<-[1..10]])

main = do
    arch <- openFile "archivo.txt" ReadMode
    contents <- hGetContents arch
    putStr contents
    hClose arch
"Debe existir la ruta archivo.txt"

        Funciones sobre Contenedores
hGetLine: Lee una linea del archivo que almacena el contenedor
    hGetLine :: Handle -> IO String

hPutStr: Escribe una cadena en el contenedor
    hPutStr :: Handle -> String -> IO ()

hPutStrLn: Escribe una cadena y da un salto de linea en el contenedor
    hPutStrLn :: Handle -> String -> IO ()

hGetChar: Lee un caracter del archivo que almacena el contenedor
    hGetChar :: Handle -> IO Char

        Funciones sobre archivos
readFile: Toma la ruta del archivo, crea un contenedor y envuelve todo el contenido
          del mismo en una accion de entrada y salida. Esta funcion, cierra
          automaticamente el contenedor asociado.
    readFile :: FilePath -> IO String

writeFile: Toma la ruta del archivo, y una cadena que escribir en el mismo
           y devuelve una accion de entrada y salida que se encargara de 
           escribir la cadena en el archivo. Si el archivo ya existe, lo sobreescribe
    writeFile :: FilePath -> String -> IO ()

appendFile: Toma la ruta del archivo, y una cadena que escribir en el mismo y devuelve
            una accion de entrada y salida que se encargara de escribir la cadena
            en el archivo
    appendFile: FilePath -> String -> IO ()

Ejemplo:
import System.IO 

-- Funcion que abre el archivo "pokemones,txt",
-- muestra su contenido en pantalla y cierra el
-- archivo

lectorDatos :: IO ()
lectorDatos = do contenedor <- openFile "pokemones.txt" ReadMode
                 contenido <- hGetContents contenedor
                 putStr contenido
                 hClose contenedor

-- Funcion principal, ejecuta las funciones correspondientes en secuencia
main :: IO ()
main = lectorDatos
-}


    