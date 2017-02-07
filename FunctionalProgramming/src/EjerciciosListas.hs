module EjerciciosListas where
import Data.List;

-- Ejercicios para practicar la recursiividad final y las expresiones lambda."Funcion Anonima"

-- a) Se pide una funci�n en Haskell que dada una lista de d�gitos devuelva 
-- una lista con todos los d�gitos hasta el primer cero.

sacaEnteros :: [Int] -> [Int]
sacaEnteros = sacaEnterosAux []

sacaEnterosAux :: [Int] -> [Int] -> [Int]
sacaEnterosAux sol [] = sol
sacaEnterosAux sol (x:xs) = if x/=0 then sacaEnterosAux (sol++[x]) xs else sol


-- b) Se pide implementar en Haskell una funci�n que dada una lista de enteros,
--  devuelva las sublistas entre cada par de ceros, respetando el orden de los
--  enteros en la lista original.

parteLista :: [Int] -> [[Int]]
parteLista = parteListaAux [] 0

parteListaAux :: [Int] -> Int -> [Int] -> [[Int]]
parteListaAux sol _ [] = [sol]
parteListaAux sol 2 xs = parteListaAux sol 0 xs
parteListaAux sol contCeros (x:xs) = if x==0 then 
												if contCeros<2 then parteListaAux sol (contCeros+1) xs 
															   else parteListaAux (sol++[x]) 0 xs 
										     else  -- x/=0
										        if contCeros==1 then parteListaAux (sol++[x]) contCeros xs 
										        				else parteListaAux (sol++[x]) (contCeros+1) xs
										                      --contCeros==0
										

-- c) Implementar una funci�n en Haskell que reciba una lista y devuelva las
-- listas productos de la secuencia de inserciones de los elementos en cada 
-- lista anterior (empezando por la lista vac�a).

segmentos :: [Int] -> [[Int]]
segmentos = segmentosAux [[]] 0

segmentosAux :: [[Int]] -> Int -> [Int] -> [[Int]] 
segmentosAux sol _ [] = sol
segmentosAux sol contCeros (x:xs) = if x==0 then 
												if contCeros<2 then segmentosAux sol (contCeros+1) xs 
															   else sol 
										     else  
										        if contCeros==1 then segmentosAux (sol++[[x]]) contCeros xs 
										        				else segmentosAux sol (contCeros+1) xs
  

-- d) Dada la siguiente definici�n de funci�n:
doble :: Int -> Int
doble = (\x -> 2*x)

-- Se pide una funci�n en Haskell que dada una lista de n�meros enteros obtenga
-- la lista resultado de calcular el doble de cada uno de los elementos de la lista.

--1) Con recursividad no final.
dobleNF :: [Int] -> [Int]
dobleNF [] = []
dobleNF (x:xs) = [doble x] ++ dobleNF xs

--2) Con recursividad final.
dobleF :: [Int] -> [Int]
dobleF lista = dobleFAux lista []

dobleFAux :: [Int] -> [Int] -> [Int]
dobleFAux [] listaSol = listaSol
dobleFAux (x:xs) listaSol = (doble x):listaSol ++ dobleFAux xs listaSol 


--3) Utilizando expresiones lambda o anonima.
dobleLambda :: [Int] -> [Int]
dobleLambda = foldr (\i listaResultado -> (doble i):listaResultado) []

--4) Utilizando la funci�n predefinida de Haskell map o de las funciones foldr o foldl.
dobleMAP :: [Int] -> [Int]
dobleMAP = map (doble) 



-- e) Dada una lista de enteros, implementa una funci�n que ordene dicha lista 
-- de menor a mayor utilizando un algoritmo de inserci�n. Dicho algoritmo 
-- recorre la lista L insertando cada elemento L[i] en el lugar correcto entre 
-- los elementos ya ordenados L[1] ,...,L[i-1].

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:[]) = [x]
ordenar (x1:x2:xs) = foldr (\i sol -> if x1>x2 then x2:x1:sol else x1:x2:sol) [] xs


