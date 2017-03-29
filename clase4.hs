-- no nos venia importando el orden de los parametros pero ahora nos empieza a importar
----csv separador = foldl1 (f separador)
----f sep acumulador palabra = acumulador ++ sep ++ palabra
-- si le pasaramos a f el separador en tercer lugar habria que escribir una nueva funcion porque no podriamos usar aplicacion parcial, ya que foldl1 f devuelve otra funcion que espera el parametro restante, es decir la lista, y si queda el separador en el medio ya no funcionaria la aplicacion parcial
-- todas las funciones de haskell reciben un parametro, porque estan currificadas


-- modelo de parcial

import Test.Hspec

data Pokemon = CPokemon {
	nivel :: Float,
	fuerza :: Float,
	vida :: Float
} deriving (Show, Eq)

data Entrenador = CEntrenador {
	nombre :: String,
	peleasGanadas :: Int,
	pokemon :: Pokemon
} deriving (Show, Eq)

type Pildora =  Pokemon -> Pokemon


-- 1)

pildoraCura :: Pildora
pildoraCura (CPokemon nivel fuerza vida) = CPokemon nivel fuerza (vida + 10)

pildoraFuerza :: Float -> Pildora
pildoraFuerza poder (CPokemon nivel fuerza vida) = CPokemon nivel (fuerza + poder) vida

pildoraSuperpower :: Pildora
pildoraSuperpower (CPokemon nivel fuerza vida) =  CPokemon nivel (fuerza * 10) (vida * 10)

tomarPildora :: Pildora  -> Pokemon -> Pokemon
tomarPildora pildora = pildora . subirDeNivel  

subirDeNivel :: Pokemon -> Pokemon
subirDeNivel pokemon = pokemon { nivel = nivel pokemon + 1}

-- 2)
golpeaA :: Pokemon -> Pokemon -> Pokemon
golpeaA atacante defensor = defensor {vida = vida defensor - danio atacante}

danio (CPokemon nivel fuerza vida) =(((2 * nivel / 5 ) + 2) * fuerza / 50) + 2 

leGanaA :: Entrenador -> Entrenador -> Bool
leGanaA local visitante = estaMuerto .  vida $ golpeaA (pokemon local) (pokemon visitante) 

-- 3)

type Torneo = [Entrenador]

esElMasFuerteDe :: Entrenador -> Torneo -> Bool
esElMasFuerteDe entrenador = all  (not . (`leGanaA` entrenador)) . sinElemento entrenador

--saca a ese elemento de la lista
sinElemento elemento lista = filter (/= elemento) lista

esMaestroPokemonDe :: Entrenador -> Torneo -> Bool 
esMaestroPokemonDe entrenador = not . estaMuerto . vida . foldr golpeaA (pokemon entrenador) . map pokemon . sinElemento entrenador 

estaMuerto :: Float -> Bool
estaMuerto vida = vida <= 0

miguel = CEntrenador {
	nombre = "Miguel",
	peleasGanadas = 10,
	pokemon = pikachu
}

nahue = CEntrenador {
	nombre = "nahue",
	peleasGanadas = 10,
	pokemon = onix
}

nico = CEntrenador {
	nombre = "nico",
	peleasGanadas = 100,
	pokemon = charizard
}

pikachu = CPokemon {
	nivel = 8,
	fuerza = 14,
	vida = 15
}

onix = CPokemon {
	nivel = 5,
	fuerza = 15,
	vida = 30
}

charizard = CPokemon {
	nivel = 15,
	fuerza = 100,
	vida = 500
}

kanto = [miguel, nahue]

hoenn = [miguel, nahue, nico]

runTests = hspec $do
	describe "Pidoras:" $do
		it "aplicar la pildora de cura" $do
			tomarPildora pildoraCura pikachu `shouldBe` CPokemon 9 14 25
		it "aplicar la pildora superpower" $do
			tomarPildora pildoraSuperpower pikachu `shouldBe` CPokemon 9 140 150
		it "subir de nivel" $do
			subirDeNivel pikachu `shouldBe`CPokemon 9 14 15 
	describe "Ataques:" $do
		it "golpeaA saca danio de vida" $do
			(pikachu `golpeaA` onix) `shouldBe` CPokemon 5 15 26.544 
		it "leGanaA funciona"	
			(miguel `shouldNotSatisfy` (`leGanaA` nahue))

	describe "Torneos:" $do
		it "nahue es el mas fuerte" $do
			nahue `shouldSatisfy` (`esElMasFuerteDe` kanto)
		it "nico esMaestroPokemonDe de hoenn" $do
			nico `shouldSatisfy` (`esMaestroPokemonDe` hoenn)
