
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
        "8" -> main
        _ -> menuAdministrativo

menuOpcionesUsuario :: IO () 
menuOpcionesUsuario = do
    putStrLn "\t1.Reservacion\n\t2.Cancelar Reservacion\n\t3.Facturar Reservacion\n\t4.Salir\n >>>Indique:"
    name <- getLine
    case name of
        "4" -> main
        _ -> menuOpcionesUsuario

