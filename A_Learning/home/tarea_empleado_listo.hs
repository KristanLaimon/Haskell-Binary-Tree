{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data Oficinista = Oficinista {
  hoursWorked:: Int,
  salaryPerHour :: Float
} 

data Comisionista = Comisionista {
  totalSales:: Float
} 

data Employee = 
  ComisionistaEmp Comisionista | 
  OficinistaEmp Oficinista 
 

concatStrs :: String -> String -> String
concatStrs a b = a ++ b

sueldo ::Employee -> Float
sueldo (ComisionistaEmp c) = totalSales c * 0.05
sueldo (OficinistaEmp o) = fromIntegral (hoursWorked o) * salaryPerHour o

main ::IO()
main =  do 
  let ruly = Oficinista {hoursWorked = 40, salaryPerHour = 28.5}
  let kris = Comisionista {totalSales = 1250} 

  putStrLn "========================"
  putStrLn "Oficinista: Ruly"
  putStrLn $ concatStrs "Horas trabajadas: " $ show $ hoursWorked ruly
  putStrLn $ concatStrs "Sueldo por hora: " $ show $ salaryPerHour ruly
  putStrLn $ concatStrs "Sueldo total: " $ show $ sueldo $ OficinistaEmp ruly
  putStrLn "========================"
  putStrLn "Comisionista: Kristan"
  putStrLn $ concatStrs "Ventas totales: " $ show $ totalSales kris
  putStrLn $ concatStrs "Porcentaje: " $ show 0.05
  putStrLn $ concatStrs "Sueldo total: " $ show $ sueldo $ ComisionistaEmp kris
  putStrLn "========================"