module EjerciciosPlegado where
-- Ejercicios para practicar con las funciones de orden superior: FOLDR y FOLDL.

-- a) Dada la siguiente definición de función, donde se recibe una 
-- lista de números enteros y los invierte:
invertirLista1 :: [Int] -> [Int]
invertirLista1 = foldr (\x lista -> lista ++ [x]) []

-- b) Analiza cuál sería el resultado de aplicar las siguientes funciones como sigue:
f1 :: [a] -> [a]
f1 = foldr (\i listaSol -> i:listaSol) []
-- Resultado: a es el elemento en la "lista" y b es la lista resultado
-- Ejemplo: f [2,3,5] FOLDR, indica que empieza por el final.
-- 5:[]
-- 3:[5]
-- 2:[3,5]
-- [2,3,5]


f2 :: [a] -> [[a]]
f2 = foldr (\i listaSol -> [i]:listaSol) [[]]
-- Ejemplo: f [2,3,5]
-- Resultado: a es el elemento en la "lista" y b es la lista resultado.
-- [5]:[]
-- [3]:[5]
-- [2]:[[3],[5]]
-- [[2],[3],[5]]


f3 = foldl (\a b -> if b==2 then 0:a else b:a) []
-- Ejemplo: f [2,3,5]
-- Resultado: FOLDL empieza por la izquierda. 2, 3 y 5. Y CAMBIAN LOS PAPELES DE A y B. (oJo)
-- b es elemento en la posicion i "lista!!i" == lista[i]
-- a es la listaSolucion. 
-- 0:[] ya que el primer elemento es un 2.
-- 3:[0]
-- 5:[3,0]
-- [5,3,0]


-- f4 = foldl (\a b -> a:b) []
-- Ejemplo: f [2,3,5]
-- Resultado: FOLDL empieza por la izquierda. 2, 3 y 5. Y CAMBIAN LOS PAPELES DE A y B. (oJo)
-- b es elemento en la posicion i "lista!!i" == lista[i]
-- a es la listaSolucion. 
-- []:2 ERROR.. deberia ser asi:  "2:[]"
-- [2]:3
-- [2,3]:5
-- [2,3,5]


-- c) Dada la siguiente definición de la función incognita, haz paso a paso
-- las reducciones aplicando la misma sobre la lista [14,5,8,7,9,16], para 
-- ver cuál es el resultado y qué hace la función.
incognita :: [Int] -> [[Int]]
incognita lista = foldl (\(y:z:xs) x -> if odd x then [y++[x],z] else [y,z++[x]]) [[],[]] lista

-- La funcion recibe una lista de numeros y separa en dos listas los numeros impares de los pares.
-- Ejemplo: incognita [14,5,8,7,9,16], FOLDL, empieza por el inicio de la lista.
--14 es par [[14],[5]]
-- 8 es par [[14, 8],[5, 7]]
-- 9 es impar [[14, 8],[5, 7]]
-- DUDAS


-- d) Se piden diferentes funciones que hagan uso de la función foldr o foldl para resolver lo siguiente:
-- 1. Reciba una lista de enteros y devuelva la suma de sus dobles.
-- FOLDR y LISTAS POR COMPRENSION
sumaDobles :: [Int] -> Int
sumaDobles lista = foldr (+) 0 [x*2 | x <- lista]

--FOLDR y MAP
sumaDobles1 :: [Int] -> Int
sumaDobles1 lista = foldr (+) 0 (map (*2) lista)

--FOLD y FUNCION ANONIMA
sumaDobles2 :: [Int] -> Int
sumaDobles2 lista = foldr (\i sol -> sol+i*2) 0 lista

-- 2. Reciba una lista de enteros y devuelva la suma de sus cuadrados.
-- FOLDR y LISTAS POR COMPRENSION
sumaCuadrados :: [Int] -> Int
sumaCuadrados lista = foldr (+) 0 [x*x | x <- lista]

--FOLDR y MAP
sumaCuadrados1 :: [Int] -> Int
sumaCuadrados1 lista = foldr (+) 0 (map (^2) lista)

--FOLDL y FUNCION ANONIMA
sumaCuadrados2 :: [Int] -> Int
sumaCuadrados2 = foldr (\i sol -> sol+i*i) 0
 
-- 3. Reciba una lista de enteros y un entero y lo inserte al final de dicha lista.
insertFinal :: [Int] -> Int -> [Int] 
insertFinal lista x = foldr (:) [x] lista
-- [1,2,3,4]   (1: (2: (3: (4:10))))
-- [1,2,3,4:10]
-- [1,2,3,4,10] 


-- 4. Reciba una lista y un número entero y devuelva dicha lista eliminando 
-- las apariciones de ese número entero.
borrarApariciones :: [Int] -> Int -> [Int]
borrarApariciones lista x = foldr (:) [] [y | y <- lista, x/=y]

