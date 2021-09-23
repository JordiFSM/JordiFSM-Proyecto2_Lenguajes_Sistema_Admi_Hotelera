import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time.Clock -- Hora
import Data.Time.Calendar -- Fecha
import Data.Time.LocalTime
import Data.Array (listArray)
type FilePath = String

--Objetivo: Cambia un string a entero
--Entrada: String
--Salida: Int
--Restricciones: Que la entrada sea string

strToInt string = do
                        let x = read string :: Integer
                        return x

--Objetivo: Pasar de int a string
--Entrada: Entero
--Salida: Un string
--Restricciones: que la entrada se un entero
intToStr  int = do
                    
                    let x = show int
                    return x

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

--------------------------------------------Tipos Habitaciones --------------------------------------------------------

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

------------------------------ cantidad habitaciones -------------------------------------------------------------

--listaHabitaciones
--Objetivo: limpiar los archivos y enviar a cargar las cantidades de habitaciones
--Entrada: --
--Salida: --
--Restricciones: --

listaHabitaciones:: IO()
listaHabitaciones = do
    lista0 <- leerArchivo"habitaciones.txt"
    let lista1 = listaHabitacionesAux lista0 []
    sobreEscribir "cantidadHabitaciones.txt" ""
    sobreEscribir "codigosHabitaciones.txt" ""
    cargarCantidades "cantidadHabitaciones.txt" lista1
    
--listaHabitacionesAux
--Objetivo: sacar cada 2 elementos de una lista en este caso los nombres y cantidades, solo crea una lista de nombres de habitaciones
--Entrada: 2 listas una será el resultado y la otra es la lista de tipos de habitaciones con nombre, descripcion y cantidades
--Salida: una lista con solo los nombres de las habitaciones
--Restricciones: que las entradas sean listas de strings

listaHabitacionesAux:: [String]->[String]->[String]
listaHabitacionesAux [] res = res
listaHabitacionesAux lista res = do
    let hd = head lista
    let tl = tail lista
    let tl1 = tail tl
    let tl2 = tail tl1
    let res2 = res++[hd] 
    listaHabitacionesAux tl2 res2

--cargarCantidades
--Objetivo: ingresar las cantidades por habitaciones a un archivo, así como enviar a generarr codigos de las habitaciones
--Entrada: un archivo y una lista
--Salida: --
--Restricciones: que el archivo exista

cargarCantidades:: System.IO.FilePath->[String]->IO()
cargarCantidades archivo [] = printearCantidadesHabitaciones
cargarCantidades archivo lista = do
    let hd = head lista
    let tl = tail lista
    let mensaje = "\tDigite la cantidad de habitaciones que tendrá la habitacion " ++ hd ++ " >>>Indique: "
    putStrLn mensaje
    cant <- getLine
    appendFile archivo (hd ++ "\n")
    appendFile archivo (cant ++ "\n")
    cant2 <- strToInt(cant)
    generarHabitaciones archivo hd cant2 0 tl "codigosHabitaciones.txt"
    
--generarHabitaciones
--Objetivo: genera cant cantidades de códigos para una habitación y los envía a escribir al archivo de códigos
--Entrada: un archivo y una lista 2 enteros y otro archivo
--Salida: --
--Restricciones: que los archivos existan

generarHabitaciones:: System.IO.FilePath->String->Integer->Integer->[String]->System.IO.FilePath->IO()
generarHabitaciones archivo nombre cant cont lista archivo2 =do 
    let cont2 = cont + 1
    pCont2 <- intToStr cont
    let mensaje = nombre++pCont2++"\n"
    if cont < cant then
        escribirCANTIDADES archivo nombre cant cont2 lista mensaje archivo2
    else
        cargarCantidades archivo lista
    
--escribirCANTIDADES
--Objetivo: escribir los códigos que se generen en el archivo de codigos
--Entrada: dos archivos los cuales son pasados entre funciones, un string para seguir generando codigos de ser necesarios 2 enteros que pararan las recurciones una lista y otro string que será un mensaje
--Salida: --
--Restricciones: que los archivos existan

escribirCANTIDADES::System.IO.FilePath->String->Integer->Integer->[String]->String->System.IO.FilePath->IO()
escribirCANTIDADES archivo nombre cant cont2 lista mensaje archivo2=do 
    appendFile archivo2 mensaje 
    generarHabitaciones archivo nombre cant cont2 lista archivo2

--printearCantidadesHabitaciones
--Objetivo: carga los archivos en 2 listas para enviarlos a la funcion de printearCantidadesAux
--Entrada: --
--Salida: --
--Restricciones: --

printearCantidadesHabitaciones::IO()
printearCantidadesHabitaciones = do
    lista1 <- leerArchivo("codigosHabitaciones.txt")
    lista2<- leerArchivo("cantidadHabitaciones.txt")
    printearCantidadesHabitacionesAux lista2 lista1

--cortarLista
--Objetivo: corta la lista cant de elementos
--Entrada: Una lista de strings y 2 enteros que detendrán las iteraciones
--Salida: --
--Restricciones: la lista cortada

cortarLista:: [String]->Int->Int->[String]
cortarLista [] cant cont = []
cortarLista lista cant cont = do
    let tl = tail lista
    let cont2 = cont+1
    if cant == cont then
        lista
    else
        cortarLista tl cant cont2

--printProximos
--Objetivo: imprimir cant de elementos de la lista 
--Entrada: Una lista de strings y 2 enteros que detendrán las iteraciones otra lista de string que será necesaria para enviar a la funcion de printearCantidadesHabitacionesAux
--Salida: --
--Restricciones: --

printProximos::[String]->Int->Int->[String]->IO()
printProximos lista cant cont lista2 = do
    let hd = head lista2
    let tl = tail lista2
    let cont2 = cont+1
    putStrLn (hd++"\n")
    if (cont == cant-1) then 
        printearCantidadesHabitacionesAux lista tl
    else
        printProximos lista cant cont2 tl

--printearCantidadesHabitacionesAux
--Objetivo: imprimir cel mensaje de la habitación por imprimir códigos
--Entrada: Dos listas de strings 
--Salida: --
--Restricciones: --

printearCantidadesHabitacionesAux::[String]->[String]->IO()
printearCantidadesHabitacionesAux [] [] = menuAdministrativo
printearCantidadesHabitacionesAux lista1 lista2 = do
    let nombre = head lista1
    let tl2 = tail lista1 
    let tl3 = tail tl2
    let cantidad = read(head tl2)::Int
    let mensaje = ("El tipo de habitacion "++nombre++" tiene las siguientes habitaciones, se muestran sus identificadores: \n")
    putStrLn mensaje
    printProximos tl3 cantidad 0 lista2
   

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
        "3" -> listaHabitaciones
        "8" -> main
        _ -> menuAdministrativo

menuOpcionesUsuario :: IO ()
menuOpcionesUsuario = do
    putStrLn "\t1.Reservacion\n\t2.Cancelar Reservacion\n\t3.Facturar Reservacion\n\t4.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "4" -> main
        _ -> menuOpcionesUsuario

