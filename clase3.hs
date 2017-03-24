-- repaso
-- Funcion (Unicidad y existencia)
-- Tipo de dato (Valores y operaciones)
-- Guardas (esconden un if)
-- Composicion
-- Recursividad (ejemplo del factorial, una funcion que se define a si misma)
-- Modelado
--	* Lista (es un conjunto, es decir, muchos elementos ordenados del mismo tipo)
--	* Tupla (para juntar elementos de distinto tipo)
--	* Data (defino mi propio tipo)
-- Pattern matching ( muy bueno para trabajar con modelado)
-- Declaratividad (digo lo que hace, en lugar de como lo hace, es decir, digo el que y no el como)
-- Imperatividad (totalmente lo contrario a la declaratividad)
-- Expresividad (que sea legible)
-- Ausencia de efecto colateral (todo tiene un estado y a traves de una operacion puedo modificar ese estado)

-- Auto: patente, color, nafta que tiene, capacidad del tanque
--data Auto = CAuto String String Float Float deriving (Show)
data Auto = CAuto {
	patente :: String,
	color :: String,
	cantNafta :: Float,
	capacidadMaxima :: Float
} deriving (Show)

unAuto = CAuto "ASD 007" "Naranja" 20 40

--patente :: Auto -> String
--patente (CAuto patente _  _  _) = patente

naftaFaltante auto = capacidadMaxima auto - cantNafta auto

--esta forma me acopla al constructor ya que si se le agrega un parametro
-- tengo que venir y modificar este constructor con otro _
naftaFaltante' (CAuto _ _ nafta capacidad) = capacidad - nafta

pintar :: Auto -> String -> Auto
pintar (CAuto patente _ nafta capacidad) color = CAuto patente color nafta capacidad

recorrer :: Auto -> Float -> Auto
recorrer (CAuto patente color nafta capacidad) kms = CAuto patente color (nafta - naftaConsumida kms) capacidad 


naftaConsumida kms = kms / 2

-- la puedo reescribir con una lambda

naftaConsumida' = (\kms -> kms / \kms -> kms /)

--dobles [] = []
--dobles (n:ns) = (2*n : dobles ns)

--triples [] = []
--triples (n:ns) = (3*n : triples ns)

--siguientes [] = []
--siguientes (n:ns) = (1+n : siguientes ns)

-- se repite mucho codigo, por ende podemos pensarlo asi

dobles [] = []
dobles (n:ns) = (doble n : dobles ns)

doble n = 2 * n

triples [] = []
triples (n:ns) = (triple n : triples ns)

triple n = 3 * n

--map funcion [] = []
--map funcion (n:ns) = (funcion n : map funcion ns) 
