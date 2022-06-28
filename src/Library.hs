module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type CorteSuprema = [Juez]
type Juez = Ley -> Bool
type Partido = String
type Tema = String

data Ley = UnaLey{
    tema :: Tema, 
    presupuesto :: Number, 
    partidos :: [Partido]
} deriving (Show,Eq)

agenda = ["Cannabis en Medicina", "Educacion Superior", "Tenis de Mesa", "Tenis"]

  
-- Ejemplos
-- Leyes:  
leyUsoMedicinalCannabis = UnaLey "Cannabis en Medicina" 5 ["Cambio de Todos", "Sector Financiero"]
leyEducacionSuperior = UnaLey "Educacion Superior" 30 ["Docentes Universitarios", "Partido centro federal"]
leyProfesionalizacionTDeMesa = UnaLey "Tenis de Mesa" 1 ["Centro Federal", "Liga de Deportistas Autonomos", "Club Paleta Veloz"]
leyTenis = UnaLey "Tenis" 2 ["Liga de Deportistas Autonomos"]
leyesDeEjemplo = [leyUsoMedicinalCannabis, leyEducacionSuperior, leyProfesionalizacionTDeMesa, leyTenis]

-- Jueces:
juezBasico ley = elem (tema ley) agenda

juezPositivo _ = True

juezInventado :: Juez
juezInventado ley = juezDePartido "Universidad Tecnologica Nacional" ley && juezDePresupuesto 5 ley

juezUltimo = juezDePresupuesto 15

supremaCorte = [juezBasico, juezDePartido "Sector Financiero", juezDePresupuesto 10, juezDePresupuesto 20, juezDePartido "Partido Conservador"]

-- Funciones generales
hayElementoEnComun lista1 lista2 = any (flip elem lista1) lista2
hayElementoEnComun' lista1 lista2 = any (`elem` lista1) lista2

substring string1 "" = False 
substring palabra1 (letra2:palabra2) = palabra1 == (take (length palabra1) palabra2) || substring palabra1 palabra2 

elPartidoApoyaLaLey partido ley = elem partido (partidos ley) 
juezDePresupuesto numero = (<= numero).(presupuesto) 
   

juezDePartido = elPartidoApoyaLaLey   
--------------------- Las Leyes
-- 1
sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles ley1 ley2 = (partidoEnComun ley1 ley2) && (mismosTemas ley1 ley2)

partidoEnComun :: Ley -> Ley -> Bool
partidoEnComun ley1 ley2 = hayElementoEnComun (partidos ley1) (partidos ley2)

mismosTemas :: Ley -> Ley -> Bool
mismosTemas ley1 ley2 = substring (tema ley1) (tema ley2  ) || substring (tema ley2) (tema ley1)


----------------------- Corte Suprema
votoCorteSuprema :: CorteSuprema -> Ley  -> Bool
votoCorteSuprema jueces ley  = cuantosVotosPositivos ley jueces >= mayoria jueces

mayoria :: CorteSuprema -> Number
mayoria jueces = div (length jueces) 2 + 1
mayoria' jueces = length jueces / 2

cuantosVotosPositivos ::  Ley -> CorteSuprema -> Number
cuantosVotosPositivos  ley jueces = 
    length (filter (\juez -> juez ley) jueces)

cuantosVotosPositivos' ley = length.filter ($ ley)


ahoraSiSonConstitucionales leyes corteSuprema nuevosJueces = 
  filter (antesNoPeroAhoraSi corteSuprema nuevosJueces) leyes

antesNoPeroAhoraSi corteSuprema nuevosJueces ley =
    not (votoCorteSuprema corteSuprema ley) && votoCorteSuprema (corteSuprema ++ nuevosJueces) ley

ahoraSiSonConstitucionales' leyes corteSuprema nuevosJueces = 
  leyesConstitucionales (corteSuprema ++ nuevosJueces) (leyesInconstitucionales corteSuprema leyes) 

leyesInconstitucionales corteSuprema leyes = leyesPorConstitucionalidad not corteSuprema leyes

leyesConstitucionales corteSuprema leyes = leyesPorConstitucionalidad id corteSuprema leyes

leyesPorConstitucionalidad funcion corteSuprema leyes = filter ((funcion.(votoCorteSuprema corteSuprema))) leyes


-------------------- Cuestion de principios

-- Las más compactas
borocotizar = map ((.) not)
borocotizar' = map (not.)
-- Con lambda
borocotizar'' = map (\juez -> (not.juez))
-- Delegando
borocotizarJuez:: Juez -> Juez
borocotizarJuez juez = not.juez

borocotizarJuez':: Juez -> Ley -> Bool
borocotizarJuez' juez ley = not (juez ley)


borocotizar''' jueces = map borocotizarJuez jueces

--leyesQueVota juez = filter (\ley -> juez ley)
--leyesQueVota' juez = filter juez

leyesQueVota = filter

juezCoincideCon juez sector leyes = all (elPartidoApoyaLaLey sector) (leyesQueVota juez leyes)