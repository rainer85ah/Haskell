module FuncionesDesplegado where

incremento :: Int -> Int
incremento x = x+1

dosVeces :: (Int -> Int) -> Int -> Int
dosVeces f x = f (f x) 

invertirLista :: [Int] -> [Int]
invertirLista = foldr (\x lista -> lista ++ [x]) []

-- a) ¿Sería correcta la siguiente definición de función para 
-- hacer lo mismo?
-- ** invertirLista = foldr (++) []
-- ** FALSO: DIFERENTES TIPOS!!


-- b) Analiza cuál sería el resultado de aplicar las 
-- siguientes funciones como sigue: Si ejecutamos f [2,3,5]

f1 = foldr (\a b-> a:b)[]
-- Salida: [2,3,5]  -- BIEN IGUALES TIPOS: []:2  ????
f2 = foldr (\a b->[a]:b)[]
-- Salida: [[2],[3],[5]] Una lista de listas!!
f3 = foldl (\a b-> if b==2 then 0:a else b:a)[]
-- Salida: [5,3,0] Invirte la posicion de los elementos. foldL
-- f4 = foldl (\a b-> a:b)[]
-- ERROR Diferetes TIPOS: 2:[] MAL!!! 


-- c) Resuelve la salida de la funcion Incognita!
-- incognita [14,5,8,7,9,16]
incognita lista = foldl (\(y:z:xs) x -> if odd x 
											then [y++[x],z]
												else [y,z++[x]])[[],[]] lista
-- En papel..

-- d) Se piden diferentes funciones que hagan uso de la
-- función foldr o foldl para resolver lo siguiente:
--1. Reciba una lista de enteros y devuelva la suma de sus 
-- dobles.


-- 2. Reciba una lista de enteros y devuelva la suma de sus
-- cuadrados.


-- 3. Reciba una lista de enteros y un entero y lo inserte
-- al final de dicha lista.


-- 4. Reciba una lista y un número entero y devuelva dicha 
-- lista eliminando las apariciones de ese número entero.

