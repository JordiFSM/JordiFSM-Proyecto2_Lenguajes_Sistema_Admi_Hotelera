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

--listaSinRepetidos
--Objetivo: tomar la lista de un archivo y guardar las terceras posiciones diferentes, es para guardar las habitaciones
-- cargadas desde el archivo, solo las que no están con nombres iguales
--Entrada: 2 listas, la lista del archivo y el resultado a devolver
--Salida: una lista de habitaciones sin repetir
--Restricciones: que las 2 entradas sean listas de strings

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

--escribirArchivoHabitaciones
--Objetivo: Escribir los strings de una lista de strings en el archivo entrante hasta que esté vacia
--Entrada: 1 path y una lista de strings
--Salida: --
--Restricciones: que las entradas sean un path de archivo y una lista de strings

escribirArchivoHabitaciones :: System.IO.FilePath->[String]->IO ()
escribirArchivoHabitaciones archivo [] = menuAdministrativo
escribirArchivoHabitaciones archivo lista = do
    let hd = head lista
    let tl = tail lista
    appendFile archivo (hd ++ "\n")
    escribirArchivoHabitaciones archivo tl

--cargarHabitaciones
--Objetivo: leer los archivos de habitaciones, limpia el de habitaciones, crea la lista de habitaciones sin repetir y las guarda nuevamente
--Entrada: --
--Salida: --
--Restricciones: --

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

--verificarFecha
--Objetivo: Validar una fecha
--Entrada: 6 Strings los cuales son dia mes año de ingreso y salida, los cambia de string a entero y valida que sean coherentes
--Salida: bool
--Restricciones: que las entradas sean 6 strings de numeros

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

--reserver
--Objetivo: Obtener todos los datos necesarios para una reservacion
--Entrada: --
--Salida: --
--Restricciones: --

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
    
--reservaporhabitacion
--Objetivo: Crea 1 lista con las cantidades de huespedes maximos por habitacion y los tipos de habitaciones
-- otra lista con las reservaciones por fecha para validar la dispomnibilidad de los tipos de habitaciones
-- y otra con las habitaciones y la cantidad de habitaciones por tipo en el hotel para pasarlas a las siguientes funciones
--Entrada: 3 listas de strings la lista que tendra todos los resultados de los datos que se reservarán, listas de fechas
--Salida: --
--Restricciones: que los 3 parametros sean 3 listas de strings

reservaporhabitacion::[String]->[String]->[String]->IO()
reservaporhabitacion res fIngreso fSalida = do
    lista0 <- leerArchivo"habitaciones.txt"
    listaHabCanHab <- leerArchivo"cantidadHabitaciones.txt" --lista de cantidad de habitaciones en hotel
    listareservasporfecha <- leerArchivo"reservacionesporFecha.txt" --lista de las reservaciones por fechas
    let listaHabHues = listaHabitacionesAux2 lista0 [] -- Lista de habitaciones y cantidad maxima de huspedes
    reservaporhabitacionAux listaHabHues res listaHabCanHab 0 fIngreso fSalida listareservasporfecha []

--reservaporhabitacionAux
--Objetivo: Obtener los datos de las cantidades de habitaciones a reservar por sus tipos, asi como las cantidades de adultos y niños, la funcion verifica la disponibilidad de las habitaciones.
--Entrada: 3 listas de strings, resultados, fechas, 1 entero que es la cantidad de huespedes total, debera coincidir con los huespedes
-- indicados al inicio, las listas creadas en la anterior funcion y una lista que contendrá las habitaciones resevadas, las cantidades y las cantidades de niños y adultos.
--Salida: --
--Restricciones: que las entradas sean 7 listas de strings y 1 entero

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
        
--identificadorReserva
--Objetivo: crear el identificador de las reservaciones
--Entrada:  1 String
--Restricciones: que la entrada sea 1 lista de strings

identificadorReserva::String->String
identificadorReserva strID = do
    let idF = "R"++strID
    idF

--validarHuespedes
--Objetivo: verificar que la cantidad de huespedes indicada al inicio, coincida con la cantidad de huespedes final
--Entrada:  5 listas de strings necesarias para seguir con las funciones de reservaciones y 1 entero que es la que hay que validar
--Restricciones: que las entradas sean 1 entero y 5 strings

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

--mostrarReservacionCorrecta
--Objetivo: mostrar los datos de la reservacion concluida, así como de guardar los datos en los archivos correspondientes
--Entrada:  5 listas de strings necesarias para mostrar los datos de una reservacion correcta.
--Restricciones: que las entradas sean 5 listas de strings

mostrarReservacionCorrecta::[String]->[String]->[String]->[String]->[String]->IO()
mostrarReservacionCorrecta res fIngreso fSalida listareservasporfecha listaReservaciones = do
    lista <- leerArchivo "codigoReservacion.txt"
    lista1 <- leerArchivo "infoHotel.txt"
    lista2 <- leerArchivo "Tarifas.txt"
    let hotelInfo = infoHotel lista1
    let strID = head lista
    let idReserva = identificadorReserva strID
    let intID = read strID::Int
    let intTotal =  calcularTotal listaReservaciones lista2 0
    let strTotal = show intTotal
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
    putStrLn ("Total:  " ++ strTotal ++ " \n")
    appendFile "reservaciones.txt" (idReserva++ "\n")
    appendFile "reservaciones.txt" (nombre++ "\n")
    appendFile "reservaciones.txt" (fechaActual++ "\n")
    appendFile "reservaciones.txt" (fechaI++ "\n")
    appendFile "reservaciones.txt" (fechaS++ "\n")
    appendFile "reservaciones.txt" (cantAd++ "\n")
    appendFile "reservaciones.txt" (cantNi ++ "\n")
    appendFile "reservaciones.txt" ("Activa" ++ "\n")
    appendFile "reservaciones.txt" (strTotal ++ "\n")
    mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones idReserva

--calcularTotal
--Objetivo: calcular los precios de las habitaciones
--Entrada: la lista de las habitaciones del hotel con sus tarifas y la lista de las reservaciones como la variable resultadoque es un entero
--Restricciones: que las entradas sean 2 listas de strings y 1 entero

calcularTotal::[String]->[String]->Int->Int
calcularTotal [] listaTarifas suma = suma
calcularTotal listaReservadas listaTarifas suma = do
    let habitacionReservada = head listaReservadas
    let habitacionTarifa = head listaTarifas
    let cantidadReservadas = read (head(tail listaReservadas))::Int
    let cantidadTarifa = read (head(tail listaTarifas))::Int
    let suma2 = suma+(cantidadReservadas * cantidadTarifa)
    if habitacionReservada == habitacionTarifa then
        calcularTotal (cortarLista listaReservadas 3 0) (cortarLista listaTarifas 1 0) suma2
    else
        calcularTotal listaReservadas (cortarLista listaTarifas 1 0) suma

--mostrarHabitacionesReservadas
--Objetivo: verificar que las cantidades de las habitaciones sean mayor a 0 para imprimirlas
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 1 string que es el id de la reservacion.
--Restricciones: que las entradas sean 5 listas de strings y 1 string
    

mostrarHabitacionesReservadas::[String]->[String]->[String]->[String]->[String]->String->IO()
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha [] id = terminarReservacion
mostrarHabitacionesReservadas  res fIngreso fSalida listareservasporfecha listaReservaciones id= do
    let nombre = head listaReservaciones 
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

--mostrarHabitacionesReservadasAux
--Objetivo: Guarda las reservaciones de las habitaciones por fecha en el archivo correspondiente
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 4 strings que es el id de la reservacion los datos a guardar y 1 int.
--Restricciones: que las entradas sean 5 listas de strings, 4 string  y 1 entero

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

--mostrarHabitacionesReservadasAux2
--Objetivo: muestra las habitaciones reservadas con los identificadores y guarda las habitaciones en el archivo.
--Entrada: 5 listas de strings necesarias para mostrar los datos de una reservacion correcta y 4 strings que es el id de la reservacion los datos a guardar y 2 int.
--Restricciones: que las entradas sean 5 listas de strings, 4 string  y 2 entero

mostrarHabitacionesReservadasAux2::[String]->[String]->[String]->[String]->[String]->String->Int->String->String->Int->String->IO()
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre 0 adultos ninos num id = mostrarHabitacionesReservadas res fIngreso fSalida listareservasporfecha listaReservaciones id
mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre cantidadINT adultos ninos num id = do
    let idHabitacion = nombre++show num
    let habitacion = "Habitacion id: " ++ idHabitacion ++" tipo: "++nombre++" "++"Cantidad de adultos: " ++ adultos ++" "++"Cantidad de ninos: " ++ ninos
    putStrLn habitacion
    appendFile "habitacionesReservadas.txt" (id++"\n")
    appendFile "habitacionesReservadas.txt" (habitacion++ "\n")
    mostrarHabitacionesReservadasAux2 res fIngreso fSalida listareservasporfecha listaReservaciones nombre (cantidadINT-1) adultos ninos (num+1) id
    
--errorCantHuespedes
--Objetivo: muestra un error de verificacion y envia al usuario a reservaciones.
--Entrada: --
--Restricciones: --

errorCantHuespedes::IO()
errorCantHuespedes = do
    putStrLn "La cantidad de huespedes indicada al inicio no coincide con la cantidad total"
    reserver

--errorSumaCantidad
--Objetivo: Mostrar un error y seguir reservando
--Entrada: 7 listas de strings, que guardan los datos de la reservacion hasta el momento
--Restricciones: que hayan 7 listas de strings y 1 entero

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
-- el archivo txt de Tarifas cada vez que se utilice esta opcion.
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
    let mensaje = "Para la habitacion de tipo " ++ hd ++ " Digite el precio de la habitacion: \n"
    putStrLn mensaje
    precio <- getLine
    appendFile "Tarifas.txt" (hd ++ "\n" ++ precio ++ "\n")
    cargarTarifaAux tl

--------------------------------------------------------------Cancelar Reservacion--------------------------------------------------------------------------
--cancelarReservacion
--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista ademas de limpiar el txt de reservaciones y pedir el ID a cancelar
--Entrada: --
--Salida: --
--Restricciones: --
cancelarReservacion::IO()
cancelarReservacion = do
    lista <-leerArchivo "reservaciones.txt"
    sobreEscribir "reservaciones.txt" ""
    putStrLn "Digite el identificador a borrar: \n"
    identificador <- getLine
    cancelarReservacionAux lista identificador

--cancelarReservacionAux
--Objetivo: Se encarga de cancelar una reservacion cambiando su estado en cancelado
--Entrada: Una lista de String y un String
--Salida: --
--Restricciones: --
cancelarReservacionAux:: [String]->String->IO()
cancelarReservacionAux [] idReser = menuOpcionesUsuario
cancelarReservacionAux lista idReser = do
    let identificador = head lista
    let tl = tail lista
    let nombreReserva = head tl
    let tl2 = tail tl
    let fechaReserva = head tl2
    let tl3 = tail tl2
    let fechaIngreso = head tl3
    let tl4 = tail tl3
    let fechaSalida = head tl4
    let tl5 = tail tl4
    let cantAdultos = head tl5
    let tl6 = tail tl5
    let cantNinos = head tl6
    let tl7 = tail tl6
    let estado = head tl7
    let tl8 = tail tl7
    let total = head tl8
    let tl9 = tail tl8
    let reservarSi = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ "Cancelado" ++ "\n"++total++"\n"
    let reservarNo = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ estado ++ "\n"++total++"\n"
    if (identificador == idReser) then
        if (estado == "Activa") then
            printearMsgCancelado reservarSi 
        else
            printearMsgNoCancelado reservarNo 
    else
        appendFile "reservaciones.txt" reservarNo
    
    cancelarReservacionAux tl9 idReser

-----------------------------------------------------------Consultar Historial de reservaciones-------------------------------------------------------------
--printearMsgCancelado
--Objetivo: Se encarga de printear el mensaje de que si se pudo cancelar y meter en el archivo de reservaciones la reservacion cancelada
--Entrada: Una lista de String
--Salida: --
--Restricciones: --
printearMsgCancelado::String->IO()
printearMsgCancelado reserva= do
    appendFile "reservaciones.txt" reserva
    let mensaje = "La reservacion fue cancelada exitosamente\n"
    putStrLn mensaje

--printearMsgNoCancelado
--Objetivo: Se encarga de printear el mensaje de no se pudo cancelar la reservacion y meter la reservacion en el archivo de reservaciones
--Entrada: Una lista de String
--Salida: --
--Restricciones: --
printearMsgNoCancelado::String->IO()
printearMsgNoCancelado reserva= do
    appendFile "reservaciones.txt" reserva
    let mensaje = "La reservacion no pudo ser cancelada porque el estado no esta en Activa\n"
    putStrLn mensaje

--mostrarHistorialReservaciones
--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --

mostrarHistorialReservaciones:: IO()
mostrarHistorialReservaciones = do
    listaReservacion <- leerArchivo "reservaciones.txt"
    mostrarHistorialReservacionesAux listaReservacion

--mostrarHistorialReservacionesAux
--Objetivo: Se encarga de mostrar el detalle de la reservacion
--Entrada: Una lista de String
--Salida: --
--Restricciones: --

mostrarHistorialReservacionesAux:: [String]->IO()
mostrarHistorialReservacionesAux [] = menuAdministrativo
mostrarHistorialReservacionesAux lista = do
    listaHabitacion <- leerArchivo "habitacionesReservadas.txt"
    let identificador = head lista
    let tl = tail lista
    let nombreReserva = head tl
    let tl2 = tail tl
    let fechaReserva = head tl2
    let tl3 = tail tl2
    let fechaIngreso = head tl3
    let tl4 = tail tl3
    let fechaSalida = head tl4
    let tl5 = tail tl4
    let cantAdultos = head tl5
    let tl6 = tail tl5
    let cantNinos = head tl6
    let tl7 = tail tl6
    let estado = head tl7
    let tl8 = tail tl7
    let total = head tl8
    let tl9 = tail tl8
    let mensaje = "Identificador: " ++ identificador ++ "\nNombre de la persona que reservo: " ++ nombreReserva ++ "\nFecha de reserva: " ++ fechaReserva ++ "\nFecha de ingreso: " ++ fechaIngreso ++ "\nFecha de salida: " ++ fechaSalida ++ "\nCantidad de adultos: " ++ cantAdultos ++ "\nCantidad de ninos: " ++ cantNinos ++ "\nEstado: " ++ estado ++ "\n"++total++"\n"
    putStrLn mensaje
    mostrarHistorialReservacionesAux2 listaHabitacion identificador
    mostrarHistorialReservacionesAux tl9

--mostrarHistorialReservacionesAux2
--Objetivo: Se encarga de mostrar el detalle de las habitaciones reservadas con el identificador de reserva
--Entrada: Una lista de String un String
--Salida: --
--Restricciones: --

mostrarHistorialReservacionesAux2::[String]->String->IO()
mostrarHistorialReservacionesAux2 [] ident = putStrLn "---------------------------------------------------------------"
mostrarHistorialReservacionesAux2 lista ident = do
    let idReservacion = head lista
    let tl = tail lista
    let info = head tl
    let tl2 = tail tl
    if(ident == idReservacion)then
        putStrLn info
    else
        putStrLn ""
    mostrarHistorialReservacionesAux2 tl2 ident

--mostrarTotalHuespedes
--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --
mostrarTotalHuespedes:: IO()
mostrarTotalHuespedes = do
    listaReservacion <- leerArchivo "reservaciones.txt"
    let total = mostrarTotalHuespedesAux listaReservacion 0
    let mensaje = "El total de huespedes es: "++show total++"\n"
    putStrLn mensaje
    menuEstadisticas

--mostrarTotalHuespedesAux
--Objetivo: Se encarga de mostrar el total de huespedes con reservaciones en estado activa o facturada
--Entrada: Una lista de String y un entero
--Salida: Un entero
--Restricciones: --
mostrarTotalHuespedesAux:: [String]->Int->Int 
mostrarTotalHuespedesAux [] res = res 
mostrarTotalHuespedesAux lista res = do
    let identificador = head lista
    let tl = tail lista
    let nombreReserva = head tl
    let tl2 = tail tl
    let fechaReserva = head tl2
    let tl3 = tail tl2
    let fechaIngreso = head tl3
    let tl4 = tail tl3
    let fechaSalida = head tl4
    let tl5 = tail tl4
    let cantAdultos = head tl5
    let tl6 = tail tl5
    let cantNinos = head tl6
    let tl7 = tail tl6
    let estado = head tl7
    let tl8 = tail tl7
    let tl9 = tail tl8
    let intCantAdultos = read(cantAdultos)::Int
    let intCantNinos = read(cantNinos)::Int
    let suma = intCantAdultos + intCantNinos
    if (estado == "Activa" || estado == "Facturada") then
        mostrarTotalHuespedesAux tl9 suma + res
    else
        mostrarTotalHuespedesAux tl9 res

-------------------------------------- Facturar --------------------------------------------------------------------------

facturar::IO()
facturar = do
    putStrLn "Indique el id de la reservacion a facturar"
    idReserva <- getLine
    lista0 <- leerArchivo "reservaciones.txt"
    let respuesta = estaActiva idReserva lista0
    if respuesta then
        facturarAux idReserva
    else
        errorFacturaNoActiva idReserva

facturarAux::String->IO()
facturarAux idReserva = do
    lista0 <- leerArchivo "codigoFactura.txt"
    let numStr = head lista0
    let numInt = (read numStr::Int) +1 
    sobreEscribir "codigoFactura.txt" (show numInt)
    let codFactura = identificadorFactura numStr
    let res = [codFactura]++[idReserva]
    putStrLn ("\nFactura #" ++ codFactura )
    putStrLn ("Codigo de reservacion: " ++ idReserva ++ " \n")
    facturarAux2 res

facturarAux2::[String]->IO()
facturarAux2 resFactura = do
    lista0 <- leerArchivo "reservaciones.txt"
    lista1 <- leerArchivo "infoHotel.txt"
    let listaReserva = detalleReservacion (head (tail resFactura)) lista0
    let reservacion = totalReservacion (head (tail resFactura)) lista0 
    let reservacionInt = read reservacion::Int 
    let infoHotelStr = infoHotel lista1
    let iVA = div reservacionInt 100*13
    let iVAStr = show iVA
    let totalStr = show (reservacionInt+iVA)
    putStrLn infoHotelStr
    putStrLn ("Subtotal de reservacion: $" ++ reservacion)
    putStrLn ("Impuestos de venta: $"++ iVAStr)
    putStrLn ("Total de reservacion: $" ++ totalStr)
    putStrLn "\n----Detalle de la reservacion----\n"
    appendFile "facturas.txt" (head resFactura++"\n"++head(tail resFactura)++"\n"++reservacion++"\n"++iVAStr++"\n"++totalStr++"\n")
    let identificador = head listaReserva
    let tl = tail listaReserva
    let nombreReserva = head tl
    let tl2 = tail tl
    let fechaReserva = head tl2
    let tl3 = tail tl2
    let fechaIngreso = head tl3
    let tl4 = tail tl3
    let fechaSalida = head tl4
    let tl5 = tail tl4
    let cantAdultos = head tl5
    let tl6 = tail tl5
    let cantNinos = head tl6
    let tl7 = tail tl6
    let estado = head tl7
    let tl8 = tail tl7
    let total = head tl8
    let tl9 = tail tl8
    let mensaje = "Identificador: " ++ identificador ++ "\nNombre de la persona que reservo: " ++ nombreReserva ++ "\nFecha de reserva: " ++ fechaReserva ++ "\nFecha de ingreso: " ++ fechaIngreso ++ "\nFecha de salida: " ++ fechaSalida ++ "\nCantidad de adultos: " ++ cantAdultos ++ "\nCantidad de ninos: " ++ cantNinos ++ "\nEstado: " ++ estado ++ "\nTotal: $"++total++"\n"
    putStrLn mensaje
    cambiarEstadoFacturado (head (tail resFactura))
    putStrLn "\nFacturado con exito!!!\n"
    menuOpcionesUsuario


--cancelarReservacion
--Objetivo: Se encarga de leer el archivo de reservaciones y hacerla una lista ademas de limpiar el txt de reservaciones y pedir el ID a cancelar
--Entrada: --
--Salida: --
--Restricciones: --
cambiarEstadoFacturado::String->IO()
cambiarEstadoFacturado idReserva = do
    lista <-leerArchivo "reservaciones.txt"
    sobreEscribir "reservaciones.txt" ""
    cambiarEstadoFacturadoAux lista idReserva

--cancelarReservacionAux
--Objetivo: Se encarga de cancelar una reservacion cambiando su estado en cancelado
--Entrada: Una lista de String y un String
--Salida: --
--Restricciones: --
cambiarEstadoFacturadoAux:: [String]->String->IO()
cambiarEstadoFacturadoAux [] idReser = menuOpcionesUsuario
cambiarEstadoFacturadoAux lista idReser = do
    let identificador = head lista
    let tl = tail lista
    let nombreReserva = head tl
    let tl2 = tail tl
    let fechaReserva = head tl2
    let tl3 = tail tl2
    let fechaIngreso = head tl3
    let tl4 = tail tl3
    let fechaSalida = head tl4
    let tl5 = tail tl4
    let cantAdultos = head tl5
    let tl6 = tail tl5
    let cantNinos = head tl6
    let tl7 = tail tl6
    let estado = head tl7
    let tl8 = tail tl7
    let total = head tl8
    let tl9 = tail tl8
    let reservarSi = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ "Facturado" ++ "\n"++total++"\n"
    let reservarNo = identificador ++ "\n" ++ nombreReserva ++ "\n" ++ fechaReserva ++ "\n" ++ fechaIngreso ++ "\n" ++ fechaSalida ++ "\n" ++ cantAdultos ++ "\n" ++ cantNinos ++ "\n" ++ estado ++ "\n"++total++"\n"
    if identificador == idReser then
        appendFile "reservaciones.txt" reservarSi
    else
        appendFile "reservaciones.txt" reservarNo
    cambiarEstadoFacturadoAux tl9 idReser

totalReservacion::String->[String]->String
totalReservacion idReserva [] = "0"
totalReservacion idReserva lista = do
    let idReservaLista = head lista
    if idReserva == idReservaLista then
        head (tail (tail (tail(tail(tail(tail(tail(tail lista))))))))
    else
        totalReservacion idReserva (cortarLista lista 8 0) 

detalleReservacion::String->[String]->[String]
detalleReservacion idReserva lista = do
    let idReservaLista = head lista
    if idReserva == idReservaLista then
         sumarSig8 lista 9 []
    else
        detalleReservacion idReserva (cortarLista lista 8 0)

sumarSig8::[String]->Int->[String]->[String]
sumarSig8 lista 0 res = res
sumarSig8 lista num res = do
    let res2 = res++[head lista]
    sumarSig8 (tail lista) (num-1) res2

errorFacturaNoActiva::String->IO()
errorFacturaNoActiva idReserva = do
    putStrLn ("La reservación "++idReserva ++ " no esta en estado activa o no existe.\n")
    facturar

identificadorFactura::String->String
identificadorFactura strID = do
    let idF = "F00"++strID
    idF

estaActiva::String->[String]->Bool 
estaActiva idReserva [] = False
estaActiva idReserva listaReservas= do
    let estado = (head(tail(tail(tail(tail(tail(tail(tail listaReservas))))))))
    let idReservaLista = head listaReservas
    if idReserva == idReservaLista then
        if estado == "Activa" then
            True 
        else
            False 
    else
        estaActiva idReserva (tail listaReservas)

--------------------------------------------Consultar Facturas-----------------------------------------------------------
--consultarFacturas
--Objetivo: Se encarga de leer el archivo de facturas y hacerla una lista
--Entrada: --
--Salida: --
--Restricciones: --
consultarFacturas:: IO()
consultarFacturas = do
    listaFacturas <- leerArchivo "facturas.txt"
    consultarFacturasAux listaFacturas

--consultarFacturasAux
--Objetivo: Se encarga de mostrar todas las facturas
--Entrada: Una lista de String
--Salida: --
--Restricciones: --
consultarFacturasAux::[String]->IO()
consultarFacturasAux [] = menuAdministrativo
consultarFacturasAux lista = do
    let numFactura = head lista
    let tl = tail lista
    let idReserva = head tl
    let tl2 = tail tl
    let subtotal = head tl2
    let tl3 = tail tl2
    let iva = head tl3
    let tl4 = tail tl3
    let total = head tl4
    let tl5 = tail tl4
    let mensaje = "Codigo de factura: "++numFactura++"\nTotal: "++total++"\nIdentificador de reserva: "++idReserva++"\n"
    putStrLn "-----------------------------------------------------------------------------------------------------------------"
    putStrLn mensaje
    consultarFacturasAux tl5

------------------------------------------------Calcular total recaudado con impuestos--------------------------------------------------

--calcularTotalRecaudado
--Objetivo: Se encarga de leer el archivo de facturas y hacerla una lista ademas de mostrar el total recaudado con impuestos
--Entrada: --
--Salida: --
--Restricciones: --
calcularTotalRecaudado:: IO()
calcularTotalRecaudado = do
    listaFacturas <- leerArchivo "facturas.txt"
    let total = calcularTotalRecaudadoAux listaFacturas 0
    let mensaje = "El total recaudado con impuestos es de $"++show total++"\n"
    putStrLn mensaje
    menuEstadisticas

--calcularTotalRecaudadoAux
--Objetivo: Se encarga de recorrer la lista de facturas y sumar los totales para retornarlo
--Entrada: Una lista de String y un entero
--Salida: Un entero
--Restricciones: --
calcularTotalRecaudadoAux:: [String]->Int->Int 
calcularTotalRecaudadoAux [] res = res 
calcularTotalRecaudadoAux lista res = do
    let numFactura = head lista
    let tl = tail lista
    let idReserva = head tl
    let tl2 = tail tl
    let subtotal = head tl2
    let tl3 = tail tl2
    let iva = head tl3
    let tl4 = tail tl3
    let total = head tl4
    let tl5 = tail tl4
    let totalInt = read(total)::Int
    calcularTotalRecaudadoAux tl5 totalInt + res
---------------------------------------Menus------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "\t1.Opcione Administrativas.\n\t2.Opciones de Usuario Normal.\n\t3.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "1"  -> menuAdministrativo
        "2" -> menuOpcionesUsuario
        "3" -> return()
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
        "5" -> mostrarHistorialReservaciones
        "6" -> consultarFacturas
        "7" -> menuEstadisticas
        "8" -> main
        _ -> menuAdministrativo

menuOpcionesUsuario :: IO ()
menuOpcionesUsuario = do
    putStrLn "\t1.Reservacion\n\t2.Cancelar Reservacion\n\t3.Facturar Reservacion\n\t4.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "1" -> reserver
        "2" -> cancelarReservacion
        "3" -> facturar
        "4" -> main
        _ -> menuOpcionesUsuario

menuEstadisticas :: IO ()
menuEstadisticas = do
    putStrLn "\t1.Total de Huespedes\n\t2.Historial de habitaciones ocupadas\n\t3.Total de habitaciones no ocupadas\n\t4.Monto recaudado con impuestos\n\t5.Volver\n >>>Indique:"
    name <- getLine
    case name of
        "1" -> mostrarTotalHuespedes
        "4" -> calcularTotalRecaudado
        "5" -> menuAdministrativo

