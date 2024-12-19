module Library where
import PdePreludat

-- 1) Crear un modelo para los turistas y crear los siguientes tres ejemplos:
-- Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
-- Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.

data Turista = UnTurista {
    cansancio :: Number,
    stress :: Number,
    estaViajandoSolo :: Bool,
    idiomas :: [String]
}deriving(Show, Eq)

ana :: Turista
ana = UnTurista 0 21 False ["espaniol"]

beto :: Turista 
beto = UnTurista 15 15 True ["aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["aleman", "catalan"]

-- 2) Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar 
-- las funciones existentes. Además:

--bajarCansancio :: Number -> Turista -> Turista
--bajarCansancio valor turista = turista {cansancio = cansancio turista - valor} 

cambiarCansancio :: Number -> Turista -> Turista
cambiarCansancio valor turista = turista {cansancio = cansancio turista + valor}

cambiarStress :: Number -> Turista -> Turista
cambiarStress valor turista = turista {stress = stress turista + valor}

--cambiarStressPorcentual' :: Number -> Turista -> Turista
--cambiarStressPorcentual' porcentaje turista = turista {stress = (100 - porcentaje) / 100 * stress turista}

cambiarStressPorcentual :: Number -> Turista -> Turista
cambiarStressPorcentual porcentaje turista = cambiarStress ((porcentaje * stress turista) `div` 100) turista

--bajarStress :: Number -> Turista -> Turista
--bajarStress valor turista = turista {stress = stress turista - valor} 

agregarIdioma :: String -> Turista -> Turista
agregarIdioma nuevoIdioma turista = turista {idiomas = nuevoIdioma : idiomas turista}

-- EXCURSIONES --

type Excursion = Turista -> Turista

-- Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.

irALaPlaya :: Excursion
irALaPlaya turista 
    | estaViajandoSolo turista = cambiarCansancio (-5) turista
    | otherwise                = cambiarStress (-1) turista 

-- Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 

apreciarAlgunElemento :: String -> Excursion
apreciarAlgunElemento elemento = cambiarStress ((-1) * length elemento) 

-- Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma = agregarIdioma 

-- Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminata, 
-- ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.

caminarCiertosMinutos :: Number -> Excursion
caminarCiertosMinutos minutos = cambiarStress ((-1) * intensidad minutos) . cambiarCansancio (intensidad minutos)

intensidad :: Number -> Number
intensidad minutos = minutos `div` 4 

-- Paseo en barco: depende de cómo esté la marea
    -- si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
    -- si está moderada, no pasa nada.
    -- si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes 
    -- alemanes.

data Marea = Fuerte | Moderada | Tranquila deriving(Show, Eq) 

-- Con esta forma queda mas prolijo (usando pattern matching)
paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte     = cambiarStress 10 . cambiarStress 6
paseoEnBarco Moderada   = id    -- me devuevle el turista sin modifiacion alguna!!
paseoEnBarco Tranquila  = salirAHablarUnIdioma "aleman" . apreciarAlgunElemento "mar" . caminarCiertosMinutos 10

--paseoEnBarco :: Marea -> Excursion
--paseoEnBarco tipoDeMarea turista 
--    | tipoDeMarea == Fuerte     = (cambiarStress 10 . cambiarStress 6) turista
--    | tipoDeMarea == Moderada   = turista
--    | tipoDeMarea == Tranquila  = (salirAHablarUnIdioma "aleman" . apreciarAlgunElemento "mar" . caminarCiertosMinutos 10) turista
--    | otherwise                 = turista

-- Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, 
-- en base a las nuevas demandas que surgen en el mercado turístico. 

-- a) Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la
-- excursión, reduce en un 10% su stress.

hacerUnaExcursion :: Excursion -> Turista -> Turista
hacerUnaExcursion excursion = cambiarStressPorcentual (-10) . excursion 

-- b) Dada la función

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho 
-- índice después de que el turista haya hecho la excursión. 
-- Llamamos índice a cualquier función que devuelva un número a partir de un turista.

-- Por ejemplo, si “stress” es la función que me da el stress de un turista:
-- > deltaExcursionSegun stress ana irALaPlaya
-- -3    --> porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)

type Indice = Turista -> Number

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerUnaExcursion excursion turista) turista

-- c) Usar la función anterior para resolver cada uno de estos puntos:

-- i) Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun cantidadIdiomas turista excursion == 1

cantidadIdiomas :: Turista -> Number
cantidadIdiomas = length .idiomas

-- ii) Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de 
-- stress al turista.

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (flip esDesestresante turista)

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante excursion turista = deltaExcursionSegun stress turista excursion <= (-3)

-- 3) Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. 
-- Un tour se compone por una serie de excursiones.

type Tour = [Excursion]

-- Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, 
-- y finaliza con una salida con gente local que habla "melmacquiano".

completo :: Tour
completo = [caminarCiertosMinutos 20, apreciarAlgunElemento "cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"] 

-- Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
-- Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, 
-- luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.

ladoB :: Excursion -> Tour
ladoB excursionAElegir = [paseoEnBarco Tranquila, excursionAElegir, caminarCiertosMinutos 120]

-- Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea 
-- al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. 
-- En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, 
-- y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionIslaVecina mareaVecina, paseoEnBarco mareaVecina]
--islaVecina Moderada  = [paseoEnBarco Moderada, irALaPlaya, paseoEnBarco Moderada]
--islaVecina Tranquila = [paseoEnBarco Tranquila, irALaPlaya, paseoEnBarco Tranquila]

excursionIslaVecina :: Marea -> Excursion
excursionIslaVecina Fuerte = apreciarAlgunElemento "lago"
excursionIslaVecina _      = irALaPlaya

-- Modelar los tours para:
-- a) Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de 
-- excursiones tenga el tour, y luego realizar las excursiones en orden.

hacerUnTour :: Turista -> Tour -> Turista 
hacerUnTour turista tour = foldl (flip hacerUnaExcursion) (cambiarStress (length tour) turista) tour
-- Hago un foldl para que lo recorre de izq a derecha (que realice las excursiones en el orden determinado por el tour)
-- La semilla del foldl en este caso, es el turista luego de que se le aumente el stress