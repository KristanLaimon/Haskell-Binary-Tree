{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
{-# HLINT ignore "Eta reduce" #-}

data Cliente = Cliente {
  nombre::String,
  edad::Int,
  saldo::Float
}

cli1 = Cliente {nombre="Kristan",edad=21,saldo=123.23}
cli2 = Cliente {nombre="Brandon",edad=20,saldo=123123.123}
cli3 = Cliente {nombre="Ana", edad=35, saldo=5432.10}
cli4 = Cliente {nombre="Carlos", edad=42, saldo=9876.54}
cli5 = Cliente {nombre="Elena", edad=28, saldo=1500.00}
cli6 = Cliente {nombre="David", edad=50, saldo=25000.75}
cli7 = Cliente {nombre="Fernanda", edad=31, saldo=7345.90}

clients :: [Cliente]
clients = [cli1, cli2, cli3, cli4, cli5, cli6, cli7]

-- ========================= LIBRARY ==============================

-- | Realiza la sumatoría de los saldos dada una lista de clientes
sumaSaldo :: [Cliente] -> Float
sumaSaldo [] = 0
sumaSaldo clientsList = foldr (\client acum -> saldo client + acum) 0 clientsList

-- | Calcula el promedio de saldos dada una lista de clientes
saldoPromedio:: [Cliente] -> Float
saldoPromedio [] = 0
saldoPromedio clientsList = sumaSaldo clientsList / fromIntegral (length clientsList)

-- función clienteMax (devolver los datos del cliente mayor, dada una lista de empleados)
clienteMax :: [Cliente] -> Cliente
clienteMax [] = error "Can't get maximum client from an empty list..."
clienteMax clientsList = maximumBy (comparing saldo) clientsList

-- | Prints full info from a client to be visible on console
clienteToString :: Cliente -> String
clienteToString client = "Name: " ++ nombre client ++ " | Age: " ++ show (edad client) ++ " | Money: " ++ show (saldo client)

consolelog :: String -> IO ()
consolelog = putStrLn

-- =========================  MAIN  ==============================
main = do
  consolelog $ "Saldo total: " ++ show (sumaSaldo clients)
  consolelog $ "Saldo promedio: " ++ show (saldoPromedio clients)
  consolelog $ "Cliente MAX: " ++ clienteToString (clienteMax clients)
