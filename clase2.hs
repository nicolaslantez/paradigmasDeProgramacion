esPar :: Int -> Bool
esPar = even
-- esPar = (mod unNumero 2) == 0

cantidadDeLetras :: String -> Int
cantidadDeLetras = length

esImpar :: Int -> Bool
esImpar = not . (esPar)

siguiente :: Int -> Int
siguiente n = n + 1

doble :: Int -> Int
doble n = n * 2

-- si hacemos :t dobleDelSiguiente podemos ver que infiere el tipo, porque Haskell es muy fuerte en inferencia
dobleDelSiguiente = doble . siguiente

vocales = ['a','e','i','o','u']

esVocal :: Char -> Bool
esVocal letra = elem letra vocales 
--esVocal letra = elem letra "aeiou"
--esVocal letra  = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'


empiezaConVocal :: String -> Bool
empiezaConVocal  = esVocal . head

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- el _ es una variable anonima es decir que no la utilizo
tieneUnElemento [_] = True
tieneUnElemento _ = False

sumaDeDosNumeros [n1,n2] = n1 + n2
sumaDeDosNumeros _ = 0

--cabeza es el 1er elemento la cola es una lista de todo lo que viene despues
-- es decir, [1,2,3] = (1:[2,3])
--dobles (cabeza:cola)
-- dobles HACER EN CASA
dobles [] = []
dobles (n:ns) = (2*n : dobles ns)

triples [] = []
triples (n:ns) = (3*n : triples ns)

primero [] = error "Sos Loco?"
primero ( cabeza : _ ) = cabeza

-- type es un alias/etiqueta
type Celular = (Int , String)

celular :: (Int,String)
celular = (1561021020, "LG G5")

--numero :: (Int, String) -> Int
--numero (x, _) = x

numero :: Celular -> Int
numero = fst

--mascota :: (Int, String)
--mascota = (14, "Kalif")


-- podemos definir algo para un tipo en particular
-- es un constructor 
data Mascota = CMascota String Int [String] deriving (Show)

kalif = CMascota "Kalif" 14 ["Rabia","Todo"]
laChanchis = CMascota "Mia" 5 []

edad (CMascota _ edad _) = edad

vacunas (CMascota _ _ vacunas) = vacunas

-- tieneVacunaContra kalif "Rabia"
tieneVacunaContra :: Mascota -> String -> Bool
tieneVacunaContra mascota vacuna = elem vacuna (vacunas mascota) 

