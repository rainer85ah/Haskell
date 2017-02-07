module FuncionesListas where
import Data.Char;

--En listas definos el tipo de cada elemento.
--Los elementos pueden ser de diferentes tipos y no sabemos el tamaño que pueda alcanzar.
--[1..5] implica que es [1,2,3,4,5]
--[1,4..] lista infinita
--['a'..'z'] implica que utiliza elementos char.. es una lista de caracteres.

-- Simbolo para añadir elementos a una lista. Es asociativo por la derecha.
-- 1:2:3:[] equivale a :
-- [3]
-- [2, 3]
-- [1, 2, 3]

--Lista de Enteros
listaEnteros1 :: [Int]
listaEnteros1 = [1,2,3,4,5]

listaEnteros2 :: [Int]
listaEnteros2 = [6,7,8,9,10]


--Operaciones con listas: deben ser del mismo tipo las listas..
-- [1,2,3] ++ [4,5] = [1,2,3,4,5]
-- concat [[1,2],[],[3,2,1]] = [1,2,3,2,1]
-- reverse “Dabale el abad” = “daba le elabaD”
-- length [2,3,6,777,8] = 5

-- head [1,2,3,4] = 1
-- last [1,2,3,4] = 4

-- tail [1,2,3,4] = [2,3,4]
-- init [1,2,3,4] = [1,2,3]

-- take 3 [1,2,3,4] = [1,2,3]
-- drop 2 [1,2,3,4] = [3,4]

-- [2,1,3,5,4]!!3 = 5 

-- Construye tuplas dentro de una lista. Del tamaño de la lista mas pequeña.
-- zip [0..3] ”hola” = [(0,’h’),(1,’o’),(2,’l’),(3,’a’)]
-- zip [0..3] ”ho” =   [(0,’h’),(1,’o’)]

-- Devuelve una lista con dos listas del mismo tipo.
-- unzip [(1,True),(4,True),(3,False)] = ([1,4,3],[True,True,False])

--EJERCICIOS:

--Hacer una funcion que recibe dos enteros y devuelve una lista de enteros(suma, resta y mult).
listaEnteros :: Int -> Int -> [Int]
--listaEnteros x y = x+y : x-y : x*y : []
listaEnteros x y = [x+y, x-y, x*y]

--Hacer una funcion que recibe dos string y si ambas miden menos o igual a tres 
-- devuelve la union y sino devulve una lista vacia.
cadena :: String -> String -> String
cadena cad1 cad2 = if(length(cad1) <= 3 &&  length(cad2) <= 3) then cad1 ++ cad2 else []


-- Listas por comprensión:
-- Lista Generadores: patron <- exp 
-- MATES: {x | x c- N, x es par} 
-- HASKELL: [x*x | x <- [1..10]]
-- [(x,y)| x <- [1..3], y <- [1..2]]

multiplicaLista :: [Int] -> [Int]
multiplicaLista x = [ x*x | x <- [1..10]]

-- Especie de FOR anidados: [x*y | x <- [1..10], y <- [1..10]]
-- Con X igual a 1, lo multiplica con toda la lista de y.
-- y pasa a ser 2 la x. y se repite el proceso.

-- Filtros
-- [E | condicion] = if condicion then [e] else []
--EJEMPLO: [ x*x | x <- [1..10], esPar x ]

--Hacer una funcion que devuelva los caracteres en mayusculas.
mayusculas :: String -> String
mayusculas cad = [x | x <- cad , isUpper x]



