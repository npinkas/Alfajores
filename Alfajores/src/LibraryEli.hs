module LibraryEli where
import PdePreludat
import Data.List (intersect)

-- ------------------------ Dominio ---------------------------------
data Alfajor = UnAlfajor {
    rellenos :: [Relleno],
    peso :: Peso,
    dulzorInnato :: Dulzor, 
    nombre :: Nombre
} deriving (Show, Eq)

data Cliente = UnCliente {
    dinero :: Number,
    alfajoresComprados :: [Alfajor],
    criterios :: [Criterio]
} deriving (Show, Eq)

-- ------------------------ Definición de Tipos ---------------------
type Peso = Number
type Dulzor = Number
type Nombre = String
type Precio = Number
type Relleno = (Nombre , Precio)
type Experimento = Alfajor -> Alfajor
type Criterio = Alfajor -> Bool

-- ----------------------- Modelado (Ejemplos) ---------------------------------
-- ---------------- Alfajores
jorgito :: Alfajor
jorgito = UnAlfajor [dulceDeLeche] 80 8 "Jorgito"

havanna :: Alfajor
havanna = UnAlfajor [mousse, mousse] 60 12 "Havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = UnAlfajor [dulceDeLeche] 40 12 "Capitan del espacio"
-- ---------------- Alfajores Cambiados
-- Punto F
jorgitito :: Alfajor
jorgitito = renombrarAlfajor "Jorgitito". abaratarAlfajor $ jorgito

jorgelin :: Alfajor
jorgelin = renombrarAlfajor "Jorgelin" . agregarCapa ( mismoRelleno jorgito) $ jorgito

capitanCostaACosta :: Alfajor
capitanCostaACosta = 
    renombrarAlfajor "Capitán del espacio de costa a costa" . hacerPremiumGrado 4 . abaratarAlfajor $ capitanDelEspacio

-- ---------------- Rellenos
dulceDeLeche :: Relleno
dulceDeLeche = ("Dulce de Leche", 12)

mousse :: Relleno
mousse = ("Mouse", 15)

fruta :: Relleno
fruta = ("Fruta", 10)

-- ---------------- Clientes
emi :: Cliente
emi = UnCliente 120 [] [buscaMarca "Capitan del espacio"]

tomi :: Cliente
tomi = UnCliente 100 [] [esPretencioso, esDulcero]

dante :: Cliente
dante = UnCliente 200 [] [antiRelleno dulceDeLeche, esExtrannio]

juan :: Cliente
juan = UnCliente 500 [] [esDulcero, buscaMarca "Jorgito", esPretencioso, antiRelleno mousse]

-- ----------------------- Funciones --------------------------------
-- ---------------- Parte 1
-- Funcion 1
coeficienteDulzor :: Alfajor -> Dulzor
coeficienteDulzor = dulzorInnato alfajor / peso alfajor

-- Funcion 2
precioAlfajor :: Alfajor -> Number
precioAlfajor alfajor = (2*) . peso alfajor + sum $ listaPrecios alfajor

listaPrecios :: Alfajor -> [Precio]
listaPrecios = map snd . rellenos

-- Funcion 3
esPotable :: Alfajor -> Bool
esPotable alfajor = 
    unaCapa alfajor && capasMismoSabor alfajor && coeficienteDulzor alfajor >= 0.1

unaCapa :: Alfajor -> Bool
unaCapa = not. null. rellenos

capasMismoSabor :: Alfajor -> Bool
capasMismoSabor alfajor = all ( (==) . head . rellenos alfajor) (tail. rellenos alfajor)

-- ---------------- Parte 2
-- Funcion A
abaratarAlfajor :: Experimento
abaratarAlfajor = reducirProp 10 peso . reducirProp 7 dulzorInnato

reducirProp :: Number -> (Alfajor -> Number) -> Alfajor -> Alfajor
reducirProp delta prop alfajor = alfajor {prop = prop alfajor - delta}

-- Funcion B
renombrarAlfajor :: Nombre -> Experimento
renombrarAlfajor newNombre alfajor = alfajor {nombre = newNombre}

-- Funcion C
agregarCapa :: Relleno -> Alfajor -> Alfajor
agregarCapa relleno alfajor = alfajor {rellenos = relleno : rellenos alfajor}

-- Funcion D
hacerPremium :: Experimento
hacerPremium alfajor
    | esPotable alfajor = cambiosPremium alfajor
    | otherwise = alfajor

cambiosPremium :: Alfajor -> Alfajor
cambiosPremium alfajor = 
    agregarCapa (mismoRelleno alfajor) . cambiarNombre " Premium" $ alfajor

mismoRelleno :: Alfajor -> Relleno
mismoRelleno = head . rellenos

cambiarNombre :: Nombre -> Alfajor -> Alfajor
cambiarNombre sufijo alfajor = alfajor { nombre = nombre alfajor ++ sufijo}

-- Funcion E
hacerPremiumGrado :: Number -> Experimento
hacerPremiumGrado 0 _ = alfajor
hacerPremiumGrado num alfajor = hacerPremium (hacerPremiumGrado (num -1) alfajor)

-- ---------------- Parte 3
-- Punto A
buscaMarca :: String -> Criterio
buscaMarca marca = estaIncluido marca . nombre 

estaIncluido :: String -> Nombre -> Bool
estaIncluido = not . null . intersect

esPretencioso :: Criterio
esPretencioso = estaIncluido "Premium" . nombre 

esDulcero :: Criterio
esDulcero = (>0.15) . coeficienteDulzor

antiRelleno :: Relleno -> Criterio
antiRelleno relleno = notelem relleno . rellenos

esExtrannio :: Criterio
esExtrannio = not . esPotable

-- Punto B
alfajoresGustados :: [Alfajor] -> Cliente -> [Alfajor]
alfajoresGustados alfajores cliente = filter (criteriosCliente cliente) alfajores

criteriosCliente :: Cliente -> Criterio
criteriosCliente cliente = foldlr (&&) True (criterios cliente)

-- Punto C
comprarAlfajor :: Cliente -> Alfajor -> Cliente
comprarAlfajor cliente alfajor 
    | puedeComprar cliente alfajor = (agregarAlfajor . gastarDinero) cliente alfajor
    | otherwise = cliente

puedeComprar :: Cliente -> Alfajor -> Bool
puedeComprar cliente alfajor = dinero cliente >= precio alfajor

agregarAlfajor :: Cliente -> Alfajor -> Cliente
agregarAlfajor cliente alfajor = cliente{alfajoresComprados = alfajor : alfajoresComprados cliente}

gastarDinero :: Cliente -> Alfajor -> Cliente
gastarDinero cliente alfajor = cliente{dinero = dinero cliente - precio alfajor}

-- Punto D
comprarAlfajoresGustados :: [Alfajores] -> Cliente -> Cliente
comprarAlfajoresGustados alfajores cliente = foldl comprarAlfajor cliente (alfajoresGustados alfajores)



