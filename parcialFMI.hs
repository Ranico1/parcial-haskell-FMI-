

data Pais = Pais {
ingresoPerCapita :: Int,
sectorPublico :: Int,
sectorPrivado :: Int,
recursosNaturales :: [RecursoNatural],
deudaExterna :: Int
}

type RecursoNatural = String 

--PUNTO 1 B 

namibia = Pais {
ingresoPerCapita = 4140,
sectorPublico = 400000,
sectorPrivado = 650000,
recursosNaturales = ["mineria", "ecoturismo"],
deudaExterna = 50000000
}

--PUNTO 2 ESTRATEGIAS DEL FMI
type Receta = Pais -> Pais 
type Dinero = Int 
prestatarlePlata :: Dinero -> Receta 
prestatarlePlata monto = mapDeudaExterna (+ (150 * monto `div` 100)) 

mapDeudaExterna :: (Int -> Int ) -> Pais -> Pais 
mapDeudaExterna f unPais = unPais {deudaExterna = f $ deudaExterna unPais}

reducirSectorPublico :: Int -> Receta 
reducirSectorPublico cantidadEliminada unPais 
    | sectorPublico unPais > 100 = (mapSectorPublico (subtract cantidadEliminada).mapIngresoPerCapita(subtract (20 * ingresoPerCapita unPais `div` 100))) unPais
    | otherwise = (mapSectorPublico (subtract cantidadEliminada).mapIngresoPerCapita(subtract (20 * ingresoPerCapita unPais `div` 100))) unPais
    
mapSectorPublico :: (Int -> Int ) -> Pais -> Pais 
mapSectorPublico f unPais = unPais {sectorPublico = f $ sectorPublico unPais}

mapIngresoPerCapita ::  (Int -> Int ) -> Pais -> Pais 
mapIngresoPerCapita f unPais = unPais {ingresoPerCapita = f $ ingresoPerCapita unPais}


regalarRecursos :: RecursoNatural -> Receta  
regalarRecursos unRecursoNatural = quitarRecurso unRecursoNatural .mapDeudaExterna (subtract 2000000)

quitarRecurso :: RecursoNatural -> Pais -> Pais 
quitarRecurso unRecursoNatural = mapRecursos (filter (/= unRecursoNatural)) 

mapRecursos :: ([RecursoNatural] -> [RecursoNatural]) -> Pais -> Pais 
mapRecursos f unPais  = unPais {recursosNaturales = f $ recursosNaturales unPais}

blindaje :: Receta 
blindaje unpais = (prestatarlePlata (pbi unpais).mapSectorPublico (subtract 500)) unpais

pbi :: Pais -> Int 
pbi unPais = ingresoPerCapita unPais * poblacionActiva unPais

poblacionActiva :: Pais -> Int 
poblacionActiva unPais = sectorPrivado unPais + sectorPublico unPais

--PUNTO 3
recetaNueva :: [Receta]
recetaNueva = [prestatarlePlata 200000000, regalarRecursos "mineria"]

-- foldl (\ pais receta -> receta pais) Namibia listaDeRecetas

--PUNTO 4
zafan :: [Pais] -> [Pais]
zafan = filter (elem "petroleo". recursosNaturales) 

deudaAfavor :: [Pais] -> Int 
deudaAfavor =  sum.map deudaExterna

-- orden superior filter con la funcion compuesta 
-- composicion elem y recursosNaturales 
-- aplicacion parcial a recursos naturales y elem

-- PUNTO 5 

mayorAmenor :: Pais -> [Receta] -> Bool 
mayorAmenor unPais [] = True
mayorAmenor unPais [x] = True 
mayorAmenor unPais (x : xs:xss) = ((pbi.x) unPais < (pbi.xs) unPais) && mayorAmenor unPais (xs:xss)



