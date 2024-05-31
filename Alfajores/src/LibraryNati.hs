module LibraryNati where
import PdePreludat


data Relleno = DulceDeLeche | Mousse | Fruta deriving (Show, Eq)
type Peso = Number
type Dulzor = Number
type Nombre = String
type Dinero = Number

data Alfajor = UnAlfajor {
    relleno :: [Relleno],
    peso :: Peso,
    dulzorInnato :: Dulzor,
    nombre :: Nombre
}deriving (Show, Eq)


jorgito = UnAlfajor [DulceDeLeche] 80 8 "Jorgito"

havanna = UnAlfajor [Mousse, Mousse] 60 12 "Havanna"

capitanDelEspacio = UnAlfajor [DulceDeLeche] 40 12 "Capitán del Espacio"

--Parte 1

type Propiedad = Alfajor -> Number

--Propiedad 1

coeficienteDeDulzor :: Propiedad
coeficienteDeDulzor alfajor = div (dulzorInnato alfajor) (peso alfajor)

--Propiedad 2

precioAlfajor :: Propiedad
precioAlfajor alfajor = doblePesoAlfajor alfajor + sumatoriaRelleno alfajor

sumatoriaRelleno ::  Alfajor -> Number
sumatoriaRelleno alfajor = sum (map precioRellenos (relleno alfajor))

doblePesoAlfajor :: Alfajor -> Number
doblePesoAlfajor = (2*) . peso

precioRellenos :: Relleno -> Number
precioRellenos relleno
    | relleno == DulceDeLeche = 12
    | relleno == Mousse = 15
    | relleno == Fruta = 10


--Propiedad 3


tieneCapasRelleno :: Alfajor -> Bool
tieneCapasRelleno = not . null . relleno

todasCapasIguales ::  Alfajor -> Bool
todasCapasIguales alfajor = all (== head (relleno alfajor)) (relleno alfajor)

coefDulzorMayorIgual :: Number -> Alfajor -> Bool
coefDulzorMayorIgual x = (>= x) . dulzorInnato

esPotable :: Alfajor-> Bool
esPotable alfajor = tieneCapasRelleno alfajor && todasCapasIguales alfajor && coefDulzorMayorIgual 0.1 alfajor

--Parte 2

-- Ej a

modificarPeso :: Peso -> Alfajor -> Alfajor
modificarPeso nuevoPeso alfajor = alfajor {peso = peso alfajor + nuevoPeso}

modificarDulzor :: Dulzor -> Alfajor -> Alfajor
modificarDulzor nuevoDulzor alfajor = alfajor {dulzorInnato = dulzorInnato alfajor + nuevoDulzor}

abaratarAlfajor :: Alfajor -> Alfajor
abaratarAlfajor = modificarDulzor (-7) . modificarPeso (-10)


-- Parte b

renombrarAlfajor :: Nombre -> Alfajor -> Alfajor
modificarNombre nuevoNombre alfajor = alfajor{nombre = nuevoNombre}

-- Parte c

agregarCapa :: Relleno-> Alfajor -> Alfajor
agregarCapa nuevoRelleno alfajor = alfajor {relleno = relleno alfajor ++ [nuevoRelleno]}

-- Parte d

obtenerCapaDelMismoTipo :: Alfajor -> Relleno
obtenerCapaDelMismoTipo alfajor = head (relleno alfajor)

agregarNombrePremium :: Alfajor -> Alfajor
agregarNombrePremium alfajor = alfajor {nombre = "Premium" ++ nombre alfajor}

modificarAlfajorPremium :: Alfajor -> Alfajor
modificarAlfajorPremium alfajor = (agregarNombrePremium . agregarCapa (obtenerCapaDelMismoTipo alfajor)) alfajor

hacerPremium :: Alfajor -> Alfajor
hacerPremium alfajor
    |esPotable = modificarAlfajorPremium alfajor
    |otherwise = alfajor

-- Parte e

hacerPremiumDeCiertoGrado :: Number ->Alfajor -> Alfajor
hacerPremiumDeCiertoGrado 1 alfajor = hacerPremium alfajor
hacerPremiumDeCiertoGrado n  alfajor = hacerPremiumDeCiertoGrado (n-1) (hacerPremium alfajor) 

--Parte f

jorgitito :: Alfajor
jorgitito = abaratarAlfajor jorgito

jorgelin :: Alfajor
jorgelin = (renombrarAlfajor "Jorgelin" . agregarCapa DulceDeLeche) jorgito

capitanCostaACosta :: Alfajor
capitanCostaACosta =  (renombrarAlfajor "Capitan del Espacio Costa a Costa" . hacerPremiumDeCiertoGrado 4 . abaratarAlfajor) capitanDelEspacio

-- Parte 3


data Cliente = UnCliente{
    dinero :: Dinero,
    alfajoresComprados :: [Alfajor],
    criterio :: [Criterio]
} deriving (Show, Eq)

type Criterio = Alfajor -> Bool

emi = UnCliente 120 [capitanDelEspacio] [contieneMarca "Capitan del Espacio"]
