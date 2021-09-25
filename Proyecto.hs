import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time.Clock 
import Data.Time.Calendar 
import Data.Time.LocalTime
import Data.Array (listArray)
import System.Console.Haskeline (Interrupt(Interrupt))
type FilePath = String

------------------------------------------------Informacion del Hotel------------------------------------------------------------------
--mostrarInfoHotel
--Objetivo: Se encarga de leer el archivo de info del hotel y mostrarla
--Entrada: --
--Salida: --
--Restricciones: --
mostrarInfoHotel:: IO()
mostrarInfoHotel = do
    lista <- leerArchivo "infoHotel.txt"
    let nombreHotel = head lista
    let tl = tail lista
    let cedulaJuridica = head tl
    let tl2 = tail tl
    let correo = head tl2
    let tl3 = tail tl2
    let telefono = head tl3
    let tl4 = tail tl3
    let pais = head tl4
    let tl5 = tail tl4
    let provincia = head tl5
    let mensaje = ("El nombre del hotel es " ++nombreHotel++ "\nLa cedula Juridica es "++cedulaJuridica++"\nEl correo es "++correo++"\nEl telefono es "++telefono++"\nEl pais es "++pais++"\nLa provincia es "++provincia++"\n")
    putStrLn mensaje
    menuAdministrativo

--infoHotel
--Objetivo: Se encarga de retornar la info del hotel
--Entrada: Una lista de Strings
--Salida: Un String
--Restricciones: --
infoHotel::[String]-> String
infoHotel lista = do
    let nombreHotel = head lista
    let tl = tail lista
    let cedulaJuridica = head tl
    let tl2 = tail tl
    let correo = head tl2
    let tl3 = tail tl2
    let telefono = head tl3
    let tl4 = tail tl3
    let pais = head tl4
    let tl5 = tail tl4
    let provincia = head tl5
    let mensaje = ("Nombre del hotel: " ++nombreHotel++ "\nCedula Juridica: "++cedulaJuridica++"\nCorreo electronico: "++correo++"\nTelefono: "++telefono++"\nPais: "++pais++"\nProvincia: "++provincia++"\n")
    mensaje

--Objetivo: Crea fecha y hora actual
--Entrada: --
--Salida: Fecha y hora 
--Restricciones: --

fecha :: IO String
fecha = do
            zC<-getCurrentTimeZone
            fC<-getCurrentTime
            let (TimeOfDay hora minuto segundo) = localTimeOfDay $ utcToLocalTime zC fC
            let (año,mes,dia) = toGregorian $ utctDay fC
            pAño<-intToStr año
            pDia<-intToStr dia
            pMes<-intToStr mes
            pHora<-intToStr hora
            pMinuto<-intToStr minuto
            pSegundo<-intToStr segundo
            let fechaT = pAño++"/"++pMes++"/"++pDia++" "++pHora++":"++pMinuto++":"++pSegundo
            return fechaT

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

--sobreEscribir
--Objetivo: Sobreescribir el archivo 
--Entrada: La ruta del archivo y un String
--Salida: --
--Restricciones: Que la ruta sea un txt
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
    lista1 <- leerArchivo"codigosHabitaciones.txt"
    lista2<- leerArchivo"cantidadHabitaciones.txt"
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
        tl
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
    if cont == cant-1 then 
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
    let mensaje = "El tipo de habitacion "++nombre++" tiene las siguientes habitaciones, se muestran sus identificadores: \n"
    putStrLn mensaje
    printProximos tl3 cantidad 0 lista2
   
--------------------------------------Reservación--------------------------------------------------------------------------------
verificarFecha::String->String->String->String->String->String->Bool
verificarFecha dia1 mes1 anio1 dia2 mes2 anio2 = do
  let anioIngreso = read anio1::Int
  let anioSalida = read anio2 ::Int
  let mesIngreso = read mes1 ::Int
  let mesSalida = read mes2 ::Int
  let diaIngreso = read dia1 ::Int
  let diaSalida = read dia2 ::Int
  if anioSalida > anioIngreso then
    True
  else
    if anioSalida == anioIngreso then
      if mesSalida > mesIngreso then
        True 
      else
        if mesSalida == mesIngreso then
          if diaSalida > diaIngreso then
            True 
          else
            False
        else
          False
    else
      False


reserver::IO()
reserver = do
    -- fecha de ingreso
    putStrLn "Indique el dia de ingreso"
    pDia <- getLine
    putStrLn "Indique el mes de ingreso"
    pMes <- getLine
    putStrLn "Indique el anio de ingreso"
    pAnio <- getLine
    let fechaIngreso = [pDia]++[pMes]++[pAnio]
    let fechaI = pDia++"/"++pMes++"/"++pAnio

    -- fecha de salida
    putStrLn "Indique el dia de salida"
    pDiaS <- getLine
    putStrLn "Indique el mes de salida"
    pMesS <- getLine
    putStrLn "Indique el anio de salida"
    pAnioS <- getLine
    let fechaSalida = [pDiaS]++[pMesS]++[pAnioS]
    let fechaS = pDiaS++"/"++pMesS++"/"++pAnioS
    -- datos extra
    putStrLn "Indique cantidad de adultos"
    pAdultos <- getLine
    putStrLn "Indique la cantidad de niños"
    pNinos <- getLine
    putStrLn "Indique el nombre de la persona que reserva"
    pNombre <- getLine
    let res = [fechaI]++[fechaS]++[pAdultos]++[pNinos]++[pNombre]
    if verificarFecha pDia pMes pAnio pDiaS pMesS pAnioS then
        reservaporhabitacion res fechaIngreso fechaSalida
    else    
        fechaInvalida 
    


reservaporhabitacion::[String]->[String]->[String]->IO()
reservaporhabitacion res fIngreso fSalida = do
    lista0 <- leerArchivo"habitaciones.txt"
    listaHabCanHab <- leerArchivo"cantidadHabitaciones.txt" --lista de cantidad de habitaciones en hotel
    listareservasporfecha <- leerArchivo"reservacionesporFecha.txt" --lista de las reservaciones por fechas
    let listaHabHues = listaHabitacionesAux2 lista0 [] -- Lista de habitaciones y cantidad maxima de huspedes
    reservaporhabitacionAux listaHabHues res listaHabCanHab 0 fIngreso fSalida listareservasporfecha []

reservaporhabitacionAux::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
reservaporhabitacionAux [] res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas = validarHuespedes cantHuesp res fIngreso fSalida listareservasporfecha habitacionesReservadas--xd res es la suma de huespedes total
reservaporhabitacionAux listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    let nombre = head listaHabHues --nombre de habitacion
    let tl = tail listaHabHues 
    let tlCanHabSinNombre = tail listaHabCanHab --cortamos la lista de las cantidades de habitaciones totales del hotel
    let totalHabs = head tlCanHabSinNombre -- tomamos la cantidad máxima de la habitacion del hotel
    let sigTotHab = tail tlCanHabSinNombre -- Siguientes habitaciones totales
    let totalHabitaciones = read totalHabs::Int --tomamos la cantidad máxima de la habitacion del hotel en Int
    let cantHues = head tl -- cantidad maxima de huespedes de habitacion en string
    let cantidadHuespedes = read cantHues::Int -- cantidad maxima de huespedes de habitacion en int
    let tl1 = tail tl  --lista de habitaciones y cantidades cortada 
    let cantidadHabitacionesReservadasFecha = listaRangoFecha nombre fIngreso fSalida listareservasporfecha --numero de habitaciones reservadas para esta fecha
    let habitacionesDisponibes = totalHabitaciones - cantidadHabitacionesReservadasFecha --habitaciones disponibles para reservar
    putStrLn ("Cantidad de habitaciones " ++ nombre ++ " a reservar: ")
    pCantTipoH <- getLine
    let pCantTipoH2 = read pCantTipoH::Int --conversion de string a int
    putStrLn ("Cantidad de adultos por habitación " ++ nombre ++ ": ")
    pCantAdTipoH <- getLine
    let pCantAdTipoH2 = read pCantAdTipoH::Int --conversion de string a int
    putStrLn ("Cantidad de niños por habitación " ++ nombre ++ ": ")
    pCantNiTipoH <- getLine
    let listaReservaciones = habitacionesReservadas++[nombre]++[pCantTipoH]++[pCantAdTipoH]++[pCantNiTipoH] 
    let pCantNiTipoH2 = read pCantNiTipoH::Int --conversion de string a int
    let pSuma = (pCantAdTipoH2 + pCantNiTipoH2)*pCantTipoH2 --suma de huespedes multiplicado por la cantidad de habitaciones
    let res2 = cantHuesp+pSuma  --Cantidad de huespedes totales de momento
    let res3 = res++[nombre]++[pCantTipoH]++[pCantAdTipoH]++[pCantNiTipoH]  --Resultado con las cantidades de la habitacion habitacion/adultos/niños
    if pSuma <= cantidadHuespedes*pCantTipoH2 then
        if habitacionesDisponibes >= pCantTipoH2 then
            reservaporhabitacionAux tl1 res3 sigTotHab res2 fIngreso fSalida listareservasporfecha listaReservaciones 
        else
            errorHabitaciones listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas
    else
        errorSumaCantidad listaHabHues res listaHabCanHab cantHuesp fIngreso fSalida listareservasporfecha habitacionesReservadas
        
identificadorReserva::String->String
identificadorReserva strID = do
    let idF = "R"++strID
    idF

validarHuespedes::Int->[String]->[String]->[String]->[String]->[String]->IO()
validarHuespedes cantHuesp resultado fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    let listasinFechas = cortarLista resultado 1 0
    let strCantAd = head listasinFechas
    let intCantAd = read strCantAd::Int
    let tl = tail listasinFechas
    let strCantNi = head tl
    let intCantNi = read strCantNi::Int 
    let sumaCant = intCantAd+intCantNi
    if sumaCant == cantHuesp then
        mostrarReservacionCorrecta resultado fIngreso fSalida listareservasporfecha habitacionesReservadas
    else
        errorCantHuespedes

mostrarReservacionCorrecta::[String]->[String]->[String]->[String]->[String]->IO()
mostrarReservacionCorrecta res fIngreso fSalida listareservasporfecha listaReservaciones = do
    lista <- leerArchivo "codigoReservacion.txt"
    lista1 <- leerArchivo "infoHotel.txt"
    let hotelInfo = infoHotel lista1
    let strID = head lista
    let idReserva = identificadorReserva strID
    let intID = read strID::Int
    let intIDNew = intID + 1
    let strIDNew = show intIDNew
    let listaNombre = cortarLista res 3 0 
    let nombre = head listaNombre
    let fechaI = head res
    let listasinFI = tail res
    let fechaS = head listasinFI
    let listasinFechas = tail listasinFI
    let cantAd = head listasinFechas
    let listasinFechasCAdultos = tail listasinFechas
    let cantNi = head listasinFechasCAdultos
    fechaActual <- fecha
    sobreEscribir "codigoReservacion.txt" strIDNew
    putStrLn "\n ******************** Factura ******************** \n"
    putStrLn hotelInfo
    putStrLn ("\nId Reservacion: " ++ idReserva ++ "\n")
    putStrLn ("Nombre de la persona que reserva: " ++ nombre ++ "\n")
    putStrLn ("Fecha en la que se realizo la reservacion: " ++ fechaActual ++" \n")
    putStrLn ("Fecha de ingreso: " ++ fechaI ++ " \n")
    putStrLn ("Fecha de salida: " ++ fechaS ++ " \n")
    putStrLn ("Cantidad de adultos: " ++ cantAd ++ " \n")
    putStrLn ("Cantida de ninos: " ++ cantNi ++ " \n")
    --mostrarTotal
    appendFile "reservaciones.txt" (idReserva++ "\n")
    appendFile "reservaciones.txt" (nombre++ "\n")
    appendFile "reservaciones.txt" (fechaActual++ "\n")
    appendFile "reservaciones.txt" (fechaI++ "\n")
    appendFile "reservaciones.txt" (fechaS++ "\n")
    appendFile "reservaciones.txt" (cantAd++ "\n")
    appendFile "reservaciones.txt" (cantNi ++ "\n")
    appendFile "reservaciones.txt" ("Activa" ++ "\n")
    --añadir total
    mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones idReserva

    
mostrarHabitacionesReservadas::[String]->[String]->[String]->[String]->[String]->String->IO()
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha [] id = terminarReservacion
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha listaReservaciones id= do
    let nombre = head listaReservaciones--
    let listaCant = tail listaReservaciones
    let cant = head listaCant --
    let cantINT = read cant::Int --
    let listaAD = tail listaCant
    let cantAd = head listaAD
    let listaNI = tail listaAD
    let cantNi = head listaNI
    let sigLista = cortarLista listaReservaciones 3 0 
    if cantINT == 0 then
        mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha sigLista id
    else
        mostrarHabitacionesReservadasAux res fIngreso fSalida listareservasporfecha sigLista nombre cantINT cantAd cantNi id

mostrarHabitacionesReservadasAux::[String]->[String]->[String]->[String]->[String]->String->Int->String->String->String->IO()
mostrarHabitacionesReservadasAux res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos id= do
    let cantidadHabitacionesReservadasFecha = listaRangoFecha nombre fIngreso fSalida listareservasporfecha --numero de habitaciones reservadas para esta fecha
    appendFile "reservacionesporFecha.txt" (head fIngreso ++"\n" ) --dia Ingreso
    appendFile "reservacionesporFecha.txt" (head (tail fIngreso)++"\n") -- mes Ingreso
    appendFile "reservacionesporFecha.txt" (head (tail (tail fIngreso))++"\n") --año Ingreso
    appendFile "reservacionesporFecha.txt" (head fSalida ++"\n")  --dia Salida
    appendFile "reservacionesporFecha.txt" (head (tail fSalida)++"\n") -- mes Salida
    appendFile "reservacionesporFecha.txt" (head (tail (tail fSalida))++"\n") --año Salida
    appendFile "reservacionesporFecha.txt" (nombre++"\n") --nombre
    appendFile "reservacionesporFecha.txt" (show cantidadINT++"\n") --nombre
    mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos cantidadHabitacionesReservadasFecha id

mostrarHabitacionesReservadasAux2::[String]->[String]->[String]->[String]->[String]->String->Int->String->String->Int->String->IO()
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre 0 adultos ninos num id = mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones id
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos num id = do
    let idHabitacion = nombre++show num
    let habitacion = "Habitacion id: " ++ idHabitacion ++" tipo: "++nombre++" "++"Cantidad de adultos: " ++ adultos ++" "++"Cantidad de ninos: " ++ ninos
    putStrLn habitacion
    appendFile "habitacionesReservadas.txt" (id++"\n")
    appendFile "habitacionesReservadas.txt" (habitacion++ "\n")
    mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre (cantidadINT-1) adultos ninos (num+1) id
    

errorCantHuespedes::IO()
errorCantHuespedes = do
    putStrLn "La cantidad de huespedes indicada al inicio no coincide con la cantidad total"
    reserver

errorSumaCantidad::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
errorSumaCantidad  listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    putStrLn "Error la cantidad de huespedes supera el limite de las habitaciones\n"
    reservaporhabitacionAux listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas

--listaRangoFecha
--Objetivo: Sacar la cantidad de habitaciones de un tipo en un determinado rango de fechas
--Entrada: 1 String nombre del tipo, 3 lista de Strings la lista de las reservaciones por fecha, las fechas de ingreso y de salida 
--Restricciones: que las entradas sean String, 3 listas de strings

listaRangoFecha::String->[String]->[String]->[String]->Int
listaRangoFecha pHabitacion fIngreso fSalida listaFechas = do
    let diaI = head fIngreso
    let diaF = head fSalida
    let diaIni = read diaI::Int
    let diaFin = read diaF::Int
    let fIngreso2 = tail fIngreso
    let mesI = head fIngreso2
    let mesIni = read mesI::Int
    listaRangoFechaAux pHabitacion listaFechas 0 diaIni diaFin mesIni 

--listaRangoFechaAux
--Objetivo: Sacar la cantidad de habitaciones de un tipo en un determinado rango de fechas
--Entrada: 1 String nombre del tipo, Una lista de Strings la lista de las reservaciones por fechas
-- 4 enteros, donde estará el resultado y se suman las habitaciones que coincidan con el rango, el dia de ingreso y de salida y mes  
--Salida: un numero con la cantidad de habitaciones reservadas en una fecha
--Restricciones: que las entradas sean String, listas de strings y 4 enteros

listaRangoFechaAux::String->[String]->Int->Int->Int->Int->Int
listaRangoFechaAux pHabitacion [] res diaIni diaFin mesIni= res
listaRangoFechaAux pHabitacion lista res diaIni diaFin mesIni  = do
    let hd = head lista
    let dia1 = read hd::Int
    let tl = tail lista
    let hd2 = head tl
    let mes1 = read hd2::Int 
    let habitaciones = cortarLista lista 5 0
    let sig = cortarLista lista 7 0
    let habitacion = head habitaciones
    let cant0 = tail habitaciones
    let cant1 = head cant0
    let cant2 = read cant1::Int
    let res2 = res+cant2
    if dia1 >= diaIni && dia1 <= diaFin && pHabitacion == habitacion && mes1 == mesIni then
        listaRangoFechaAux pHabitacion sig res2 diaIni diaFin mesIni 
    else
         listaRangoFechaAux pHabitacion sig res diaIni diaFin mesIni 

--listaHabitacionesAux2
--Objetivo: sacar cada 1 elemento de una lista en este caso los nombres y cantidades, solo crea una lista de nombres de habitaciones
--Entrada: 2 listas una será el resultado y la otra es la lista de tipos de habitaciones con nombre, descripcion y cantidades
--Salida: una lista con solo los nombres y cantidades maximas de huespedes de las habitaciones
--Restricciones: que las entradas sean listas de strings

listaHabitacionesAux2:: [String]->[String]->[String]
listaHabitacionesAux2 [] res = res
listaHabitacionesAux2 lista res = do
    let hd = head lista
    let tl = tail lista
    let tl1 = tail tl
    let tl2 = tail tl1
    let res2 = res++[hd]++[head tl1]
    listaHabitacionesAux2 tl2 res2

--errorHabitaciones
--Objetivo: muestra un error en pantalla y vuelve a las reservaciones por habitación
--Salida: --
--Restricciones: que las entradas sean 7 listas de strings y 1 entero

errorHabitaciones::[String]->[String]->[String]->Int->[String]->[String]->[String]->[String]->IO()
errorHabitaciones  listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas = do
    putStrLn "Cantidad de habitaciones insuficiente para esta fecha.\n"
    reservaporhabitacionAux listaHabHues res listaHabCanHab cantHues fIngreso fSalida listareservasporfecha habitacionesReservadas

--fechaInvalida
--Objetivo: muestra un error en pantalla y vuelve a las reservaciones 
--Salida: --
--Restricciones: --

fechaInvalida ::IO ()
fechaInvalida = do
        putStrLn "La fecha fue incorrecta"
        reserver
        
--terminarReservacion
--Objetivo: muestra en pantalla un mensaje de exitoso y vuelve al menu de opciones de usuario 
--Salida: --
--Restricciones: --

terminarReservacion:: IO()
terminarReservacion = do
    putStrLn "\n  +++++++++++++++++++++++++++++++++++\n++++++++++Reservacion Exitosa!!++++++++\n  +++++++++++++++++++++++++++++++++++\n"
    menuOpcionesUsuario

---------------------------------------------Cargar Tarifa--------------------------------------------------------------

-- cargarTarifa
--Objetivo: cargar el archivo txt de habitaciones y transformalo en una lista con solo los nombres de las habitaciones, reseta el
--          el archivo txt de Tarifas cada vez que se utilice esta opcion.
--Entrada: --
--Salida: --
--Restricciones: --

cargarTarifa::IO()
cargarTarifa = do
    sobreEscribir "Tarifas.txt" ""
    lista0 <- leerArchivo"habitaciones.txt"
    let lista1 = listaHabitacionesAux lista0 []
    cargarTarifaAux lista1

-- cargarTarifaAux
--Objetivo: Se encarga de pedir el precio de tarifa por cada tipo de habitacion al usuario y agregarlo a un archivo txt
--Entrada: Una lista de String
--Salida: --
--Restricciones: --
cargarTarifaAux ::[String]->IO()
cargarTarifaAux [] = menuAdministrativo 
cargarTarifaAux lista = do
    let hd = head lista
    let tl = tail lista
    let mensaje = ("Para la habitacion de tipo " ++ hd ++ " Digite el precio de la habitacion: \n")
    putStrLn mensaje
    precio <- getLine
    appendFile "Tarifas.txt" (hd ++ "\n" ++ precio ++ "\n")
    cargarTarifaAux tl

---------------------------------------Menus------------------------------------------------------------------------------
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
        "1" -> mostrarInfoHotel
        "2" -> cargarHabitaciones
        "3" -> listaHabitaciones
        "4" -> cargarTarifa
        "8" -> main
        _ -> menuAdministrativo

menuOpcionesUsuario :: IO ()
menuOpcionesUsuario = do
    putStrLn "\t1.Reservacion\n\t2.Cancelar Reservacion\n\t3.Facturar Reservacion\n\t4.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "1" -> reserver
        "4" -> main
        _ -> menuOpcionesUsuario


