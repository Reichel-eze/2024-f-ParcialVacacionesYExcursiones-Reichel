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

-- b) Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. 
-- Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla

algunConvincente :: Turista -> [Tour]-> Bool
algunConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (\excursion -> esDesestresante excursion turista || loDejaAcompaniado excursion turista)

esConvincente' :: Turista -> Tour -> Bool
esConvincente' turista = any (`loDejaAcompaniado` turista) . excursionesDesestresantes turista
-- 1ero. FIltro las excursiones desestresantes del tour
-- 2dos. Tengo que consultar si existe alguna excursion desestresante que deja al turista acompañado

loDejaAcompaniado :: Excursion -> Turista -> Bool
loDejaAcompaniado excursion = not . estaViajandoSolo . hacerUnaExcursion excursion 

-- c) Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad 
-- recibida de cada turista a quienes les resultó convincente el tour. 
-- La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.

efectividadDeUnTour :: Tour -> [Turista] -> Number
efectividadDeUnTour tour = sum . map (flip espiritualidad tour) . filter (flip esConvincente tour)
-- 1ero. Filtro a los turistas que les resulto convincente el tour
-- 2dos. De cada uno de ellos obtengo la espiritualidad y lo pongo en una lista 
-- 3ero. Hago la suamtoria de la lista 

espiritualidad :: Turista -> Tour -> Number
espiritualidad turista = negate . sum . map (espiritualidadExcursion turista) 

espiritualidadExcursion :: Turista -> Excursion -> Number
espiritualidadExcursion turista excursion = deltaExcursionSegun stress turista excursion + deltaExcursionSegun cansancio turista excursion

--- Otra VERSION (usando los deltas)

efectividadDeUnTour' :: Tour -> [Turista] -> Number
efectividadDeUnTour' tour = sum . map (espiritualidadTour tour) . filter (flip esConvincente tour)
-- 1ero. Filtro a los turistas que les resulto convincente el tour
-- 2dos. De cada uno de ellos obtengo la espiritualidad y lo pongo en una lista 
-- 3ero. Hago la sumatoria de la lista 

-- Recordar que los deltas son negativos por ser perdidas de stress y cansancion (por lo tanto entonces hice suma de negativos)
-- Entonces pongo un (-1) * para pasarlo a positivo la espiritualidad
espiritualidadTour :: Tour -> Turista -> Number
espiritualidadTour tour turista = (-1) * deltaTourSegun espiritualidad' turista tour

-- Es como el deltaExcursionSegun pero con un tour!!
deltaTourSegun :: Indice -> Turista -> Tour -> Number
deltaTourSegun indice turista tour = deltaSegun indice (hacerUnTour turista tour) turista

-- Como si fuese una nueva caracteristica de los turistas!!
espiritualidad' :: Indice
espiritualidad' turista = stress turista + cansancio turista

-- 4) Implementar y contestar en modo de comentarios o pruebas por consola
-- a. Construir un tour donde se visiten infinitas playas.

tourPlayasInfinitas :: Tour
tourPlayasInfinitas = repeat irALaPlaya

tourPlayasInfinitas' :: Tour
tourPlayasInfinitas' = irALaPlaya : tourPlayasInfinitas'

-- b. ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.

-- esConvincente' :: Turista -> Tour -> Bool
-- esConvincente' turista = any (`loDejaAcompaniado` turista) . excursionesDesestresantes turista

-- excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
-- excursionesDesestresantes turista = filter (flip esDesestresante turista)

-- esDesestresante :: Excursion -> Turista -> Bool
-- esDesestresante excursion turista = deltaExcursionSegun stress turista excursion <= (-3)

-- > esConvincente' ana tourPlayasInfinitas
-- True

-- Para ana es convinciente porque para que el tour sea convincente necesita por lo menos alguna excursion que sea desestresante
-- y luego de dicha excursion la deja acompañada a ana (como ana en un comienzo no estaba viajando sola, entonces no afecta)
-- (Ya la priemra excursion del tour cumple con ser desestresante y no afecta a la varible de viajar solo, CUMPLE) 

-- > esConvincente' beto tourPlayasInfinitas
-- ERROR

-- Mientras tanto para beto no podemos llegar a un resultado ya que haskell/algoritmo se quedara buscando infinitamente alguna
-- excursion que cumpla con ambas condiciones (que sea desestresante y que lo deje acompañado a beto). En este caso debido a que
-- beto en un comienzo NO se encuentra acompañado (esta viajando solo) entonces haskell intentara buscar alguna excursion que
-- modifique dicha condicion, a su vez buscara tambien que sea desestresante   

-- c. ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

-- No es imposibe, porque para calcular la efectividad de un tour necesito la espiritualidad obtenida por cada turista luego
-- de realizar el tour (y como el tour es infinito no puedo llegar a un resultado concreto de espitualidad). La unica manera
-- de llegar a un resultado es si doy una lista vacia de turistas, dando como resultado una efectividad = 0  

-- > efectividadDeUnTour' tourPlayasInfinitas []
-- 0