import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time.Clock -- Hora
import Data.Time.Calendar -- Fecha
import Data.Time.LocalTime
import Data.Array (listArray)
type FilePath = String

sobreEscribir :: System.IO.FilePath -> String -> IO ()
sobreEscribir archivo algo = do
    writeFile archivo (algo)

--Leer archivo
--Objetivo: Lee un archivo txt
--Entrada: La ruta de un archivo
--Salida:Lista con lo que se encuentre en el archivo
--Restricciones: Que la ruta sea un txt
leerArchivo :: System.IO.FilePath -> IO [String]
leerArchivo (archivo) = do
    contents <- readFile archivo
    evaluate (force contents)
    let lineas = lines contents
    return lineas;

--Escribir archivo
--Objetivo: Escribe en un archivo
--Entrada: La ruta de un archivo y lo que quiere escribir
--Salida: La escritura de "algo" dentro del archivo
--Restricciones: Entre un fichero txt y String lo que escriba
escribirArchivo :: (System.IO.FilePath, [Char]) -> IO ()
escribirArchivo (archivo, algo) = do
    appendFile archivo (algo ++ "\n")

estaLista :: String->[String]->Bool
estaLista nombre [] = False
estaLista nombre lista = do
    let hd = head lista
    let tl = tail lista
    if hd == nombre then
        True
    else
        estaLista nombre tl

listaSinRepetidos :: [String] -> [String] -> Int ->[String]
listaSinRepetidos [] res opcion = res
listaSinRepetidos lista res opcion = do
    let tl = tail lista
    let tl1 = tail tl
    let tl2 = tail tl1
    let tl3 = tail tl2
    let hd = head lista
    let res2 = res ++ [hd]
    let opcion1 = opcion+1
    let opcion2 = opcion+3
    (if ((mod opcion  3) == 0) && estaLista hd res then
         listaSinRepetidos tl2 res opcion2
    else
        listaSinRepetidos tl res2 opcion1)

escribirArchivoHabitaciones :: System.IO.FilePath->[String]->IO ()
escribirArchivoHabitaciones archivo [] = menuAdministrativo
escribirArchivoHabitaciones archivo lista = do
    let hd = head lista
    let tl = tail lista
    appendFile archivo (hd ++ "\n")
    escribirArchivoHabitaciones archivo tl

cargarHabitaciones:: IO()
cargarHabitaciones = do
    lista0 <- leerArchivo"habitaciones.txt"
    lista1 <- leerArchivo"tiposH.txt"
    let lista2 = listaSinRepetidos lista1 lista0 0
    sobreEscribir "habitaciones.txt" ""
    escribirArchivoHabitaciones "habitaciones.txt" lista2
    menuAdministrativo



-------------------------------Menus------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "\t1.Opcione Administrativas.\n\t2.Opciones de Usuario Normal.\n\t3.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "1"  -> menuAdministrativo
        "2" -> menuOpcionesUsuario
        _ -> main

menuAdministrativo :: IO ()
menuAdministrativo = do
    putStrLn "\t1.Informacion de Hotel\n\t2.Cargar Tipo de Habitaciones\n\t3.Asignar cantidad de habitaciones por tipo\n\t4.Cargar Tarifas\n\t5.Consultar reservaciones\n\t6.Consultar Facturas\n\t7.Estadisticas de Ocupacion\n\t8.Volver\n >>>Indique:"
    name <- getLine
    case name of
        "2" -> cargarHabitaciones
        "8" -> main
        _ -> menuAdministrativo

menuOpcionesUsuario :: IO ()
menuOpcionesUsuario = do
    putStrLn "\t1.Reservacion\n\t2.Cancelar Reservacion\n\t3.Facturar Reservacion\n\t4.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "4" -> main
        _ -> menuOpcionesUsuario

