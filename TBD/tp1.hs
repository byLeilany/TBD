module MapReduce where

import Data.Ord
import Data.List
import Test.HUnit
import Data.Function 

----------------------------------Integrantes----------Taller 1---------------------------
--Bramati, Bianca (LU 1893/21)
--Melendez Rangel, Daniela Leilany del Carmen (102/20)
--Tiracchia, Thiago (LU 1502/21)
-----------------------------------Sección 1---------Diccionario ---------------------------
type Dict k v = [(k,v)]

-- Ejercicio 1
belongs :: Eq k => k -> Dict k v -> Bool
belongs e = foldr (\x r -> (fst x)== e || r) False

(?) :: Eq k => Dict k v -> k -> Bool
k ? v = belongs v k

-- Ejercicio 2
get :: Eq k => k -> Dict k v -> v
get e d = snd (foldr (\x r -> if fst x==e then x else r) (head d) d)

(!) :: Eq k => Dict k v -> k -> v
d ! e = get e d 

-- Ejercicio 3: inserto (clave, valor) en el Dict cuando clave NO belongs. Y caso then: busco con foldr dentro del dict hasta llegar a elemento en dict con clave e, 
-- hago la funcion f con val y valor actual de ese elemento, el resto de elementos del Dict los dejo igual
insertWith :: Eq k => (v -> v -> v) -> k -> v -> Dict k v -> Dict k v
insertWith f key val d = if (d ? key) then aplicoF else (key,val):d 
            where aplicoF = foldr (\(a, b) r -> if a==key then (a,(f (b) val)):r else (a, b):r) [] d

-- Ejercicio 4
groupByKey :: Eq k => [(k,v)] -> Dict k [v]
groupByKey  = foldr (\(a,b) r -> insertWith (++) a [b] r) [] 

-- Ejercicio 5 : Junto los 2 diccionarios y combino sus claves, y como esto hace que me queden tuplas con una lista, le aplico f a cada lista de cada tupla
unionWith :: Eq k => (v -> v -> v) -> Dict k v -> Dict k v -> Dict k v
unionWith f d1 d2 = map (fEnSecond f) (groupByKey (d1 ++ d2))
--Hacemos un map con fEnSecond f ya que groupByKey nos devuelve Dict (k,[v]) y queremos Dict (k, v) combinando los valores de [v] según f. 
fEnSecond :: (v -> v -> v) -> (k,[v]) -> (k,v)
fEnSecond f (a,b) = (a, (foldr1 (\x r-> f r x)) b )


---------------------------------Sección 2--------------MapReduce---------------------------

type Mapper a k v = a -> [(k,v)]
type Reducer k v b = (k, [v]) -> [b]

-- Ejercicio 6: Repartimos en una forma circular en la lista, vamos rotando (traigo el ultimo elem, le agrego el x, y la envio al final)
distributionProcess :: Int -> [a] -> [[a]]
distributionProcess cant l =  foldr (\x r -> (x:(last r)):(init r)) (replicate cant []) l  

-- Ejercicio 7
mapperProcess :: Eq k => Mapper a k v -> [a] -> Dict k [v]
mapperProcess m l = groupByKey (concat (map m l))

-- Ejercicio 8 
combinerProcess :: (Eq k, Ord k) => [Dict k [v]] -> Dict k [v]
combinerProcess dic = sortBy (on compare fst) combined
                where combined = foldr1 (\x r -> unionWith (++) x r) dic

-- Ejercicio 9
reducerProcess :: Reducer k v b -> Dict k [v] -> [b]
reducerProcess r d = concat (map r d) 

-- Ejercicio 10
mapReduce :: (Eq k, Ord k) => Mapper a k v -> Reducer k v b -> [a] -> [b]
mapReduce m r l = reducerProcess r (combinerProcess (map (mapperProcess m) (distributionProcess 100 l)))

--Funciones de prueba --

--Restos módulo 5

mapperRestos :: Mapper Int Int Int
mapperRestos n = [(n `mod` 5, n)]

reducerRestos :: Reducer Int Int (Int, Int)
reducerRestos (r, ns) = [(r, length ns)]

restosMod5 :: [Int] -> Dict Int Int
restosMod5 = mapReduce mapperRestos reducerRestos

--Clasificación de palabras
palabras :: [[(Int, [[Char]])]]
palabras = [[(1,["Hola","Chau"]),(2,["Perro","Gato"])],[(2,["Jirafa"])],[(3,["Casa"]),(4,["Tren", "Auto"]), (1, ["Saludos"])],[(2, ["Perro"]), (4, ["Barco"])]]

-- Monumentos por país

mapperMPP :: Mapper (Structure, Dict String String) String ()
mapperMPP (Monument, metadata) = let country = metadata ! "country"
                                 in [(country, ())]
mapperMPP _                  = []

reducerMPP :: Reducer String () (String, Int)
reducerMPP (country, units) = [(country, length units)]

monumentosPorPais :: [(Structure, Dict String String)] -> [(String, Int)]
monumentosPorPais = mapReduce mapperMPP reducerMPP

-- Monumentos top

mapperVPM :: Mapper String String ()
mapperVPM m = [(m, ())]

reducerVPM :: Reducer String () (String, Int)
reducerVPM (monument, units) = [(monument, length units)]

visitasPorMonumento :: [String] -> [(String, Int)]
visitasPorMonumento = mapReduce mapperVPM reducerVPM

mapperOPV :: Mapper (String, Int) Int String
mapperOPV (monument, visitCount) = [(-visitCount, monument)]

-- Acá se utiliza el orden por visitCount que provee mapReduce.
-- Sencillamente se descarta el número para devolver la lista
-- de monumentos ordenada por visitas.
reducerOPV :: Reducer Int String String
reducerOPV = snd

ordenarPorVisitas :: [(String, Int)] -> [String]
ordenarPorVisitas = mapReduce mapperOPV reducerOPV

monumentosTop :: [String] -> [String]
monumentosTop = ordenarPorVisitas.visitasPorMonumento
                

-- ------------------------ Ejemplo de datos para pruebas ----------------------
data Structure = Street | City | Monument deriving Show

items :: [(Structure, Dict String String)]
items = [
    (Monument, [
      ("name","Obelisco"),
      ("latlong","-36.6033,-57.3817"),
      ("country", "Argentina")]),
    (Street, [
      ("name","Int. Güiraldes"),
      ("latlong","-34.5454,-58.4386"),
      ("country", "Argentina")]),
    (Monument, [
      ("name", "San Martín"),
      ("country", "Argentina"),
      ("latlong", "-34.6033,-58.3817")]),
    (City, [
      ("name", "Paris"),
      ("country", "Francia"),
      ("latlong", "-24.6033,-18.3817")]),
    (Monument, [
      ("name", "Bagdad Bridge"),
      ("country", "Irak"),
      ("new_field", "new"),
      ("latlong", "-11.6033,-12.3817")])
    ]


------------------------------------------------
------------------------------------------------

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio8" ~: testsEj9,
  "ejercicio8" ~: testsEj10
  ]
  
testsEj1 = test [
  ([("calle",[3]),("ciudad",[2,1])] ? "ciudad")  ~=? True,
  ([("calle",[3]),("ciudad",[2,1])] ? "perro")  ~=? False --Agregar sus propios tests.
  ]

testsEj2 = test [
  [("calle","San Blas"),("ciudad","Hurlingham")] ! "ciudad" ~=? "Hurlingham" --Agregar sus propios tests.
  ]

testsEj3 = test [
  (insertWith (++) 1 [99] [(1, [1]), (2, [2])]) ~=? [(1,[1,99]),(2,[2])] --Agregar sus propios tests.
  ]

testsEj4 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj5 = test [
  (unionWith (+) [("rutas",3)] [("rutas", 4), ("ciclos", 1)]) ~=? [("rutas",7),("ciclos",1)] --Agregar sus propios tests.
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  mapperProcess mapperRestos [1, 5, 10, 25, 3, 14, 4] ~=? [(4,[4,14]),(3,[3]),(0,[25,10,5]),(1,[1])] --Agregar sus propios tests.
  ]

testsEj8 = test [
  (map (\(x,y)->(x,sort y)) $ combinerProcess palabras) ~=? [(1,["Chau","Hola","Saludos"]),(2,["Gato","Jirafa","Perro","Perro"]),(3,["Casa"]),(4,["Auto","Barco","Tren"])]
 --Agregar sus propios tests.
  ]

testsEj9 = test [
  reducerProcess (\(x, xs)->x : nub xs)  [("Saludo:",["Chau","Hola","Saludos"]),("Mamífero:",["Gato","Jirafa","Perro","Perro"]),("Edificio:",["Casa"]),("Vehículo:",["Auto","Barco","Tren"])] ~=? ["Saludo:","Chau","Hola","Saludos","Mamífero:","Gato","Jirafa","Perro","Edificio:","Casa","Vehículo:","Auto","Barco","Tren"] --Agregar sus propios tests.
  ]

testsEj10 = test [
  sort (visitasPorMonumento ["m1","m2","m3","m2"]) ~=? [("m1",1),("m2",2),("m3",1)],
  [("Argentina",2),("Irak",1)] ~=? sort (monumentosPorPais items),
  monumentosTop ["m3","m2","m2","m3","m1","m2","m3","m3","m4","m1"] ~=? ["m3","m2","m1","m4"] --Agregar sus propios tests.
  ]
