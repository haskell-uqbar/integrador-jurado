module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type CorteSuprema = [Juez]
type Juez = Ley -> Bool
type Sector = String
type Tema = String

data Ley = UnaLey{
    tema :: Tema, 
    presupuesto :: Number, 
    sectores :: [Sector]
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
juezInventado ley = juezDeSector "Universidad Tecnologica Nacional" ley && juezDePresupuesto 5 ley

juezUltimo = juezDePresupuesto 15

supremaCorte = [juezBasico, juezDeSector "Sector Financiero", juezDePresupuesto 10, juezDePresupuesto 20, c "Partido Conservador"]

-- Funciones generales
hayElementoEnComun lista1 lista2 = any (flip elem lista1) lista2
hayElementoEnComun' lista1 lista2 = any (`elem` lista1) lista2
hayElementoEnComun'' lista1 lista2 = any (\x -> elem x lista1) lista2

substring string1 "" = False 
substring palabra1 (letra2:palabra2) = palabra1 == (take (length palabra1) palabra2) || substring palabra1 palabra2 

elSectorApoyaLaLey partido ley = elem sector (sectores ley) 
elSectorApoyaLaLey' partido = elem sector.sectores 

juezDePresupuesto numero ley = presupuesto ley <= numero
juezDePresupuesto' numero = (<= numero).presupuesto 

juezDeSector = elSectorApoyaLaLey   
--------------------- Las Leyes
-- 1
sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles ley1 ley2 = sectoresEnComun ley1 ley2 && mismosTemas ley1 ley2

sectoresEnComun :: Ley -> Ley -> Bool
sectoresEnComun ley1 ley2 = hayElementoEnComun (sectores ley1) (sectores ley2)

mismosTemas :: Ley -> Ley -> Bool
mismosTemas ley1 ley2 = substring (tema ley1) (tema ley2  ) || substring (tema ley2) (tema ley1)


----------------------- Corte Suprema
votoCorteSuprema :: CorteSuprema -> Ley  -> Bool
votoCorteSuprema jueces ley  = cuantosVotosPositivos ley jueces >= mayoria jueces

mayoria :: CorteSuprema -> Number
mayoria jueces = div (length jueces) 2 + 1
mayoria' jueces = length jueces / 2

cuantosVotosPositivos ::  Ley -> CorteSuprema -> Number
cuantosVotosPositivos  ley jueces = length (filter (\juez -> juez ley) jueces)

cuantosVotosPositivos' ley = length.filter ($ ley) 
cuantosVotosPositivos'' ley jueces = length (filter ($ ley) jueces)
cuantosVotosPositivos''' ley jueces = length (filter (vota ley) jueces)
vota ley juez = juez ley


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
borocotizar = map (not.)
borocotizar' jueces = map (not.) jueces
-- Con lambda
borocotizar'' jueces = map (\juez -> (not.juez)) jueces
-- Delegando
borocotizar''' jueces = map borocotizarJuez jueces
borocotizar'''' = map borocotizarJuez

borocotizarJuez:: Juez -> Juez
borocotizarJuez juez = not.juez

borocotizarJuez':: Juez -> Ley -> Bool
borocotizarJuez' juez ley = not (juez ley)

leyesQueVota = filter
leyesQueVota' juez = filter (\ley -> juez ley)
leyesQueVota'' juez = filter juez

juezCoincideCon juez sector leyes = all (elSectorApoyaLaLey sector) (leyesQueVota juez leyes)
