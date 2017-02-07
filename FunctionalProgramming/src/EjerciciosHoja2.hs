-- Utilizar la recursividad final y otros son para practicar con las expresiones lambda.
module EjerciciosHoja2 where
import Data.Char;
import Data.List;

-- A) Implementa una funci�n en Haskell que elimine de una lista de 
-- enteros aquellos n�meros m�ltiplo de x.

-- Con definici�n de listas por comprensi�n
cribaMultiplos :: [Int] -> Int -> [Int]
cribaMultiplos [] _ = []
cribaMultiplos (x:xs) b = [x | x <- xs, mod x b > 0]

-- Con recursividad no final
cribaMultiplosR :: [Int] -> Int -> [Int]
cribaMultiplosR [] _ = []
cribaMultiplosR (x:xs) b = if ((mod x b) == 0) then cribaMultiplosR xs b else [x]++cribaMultiplosR xs b

-- Con recursividad final
cribaMultiplosRF :: Int -> [Int] -> [Int]
cribaMultiplosRF b = recursividad b []

recursividad :: Int -> [Int] -> [Int] -> [Int]
recursividad _ listaSol [] = listaSol
recursividad b listaSol (x:xs) = if ((mod x b) == 0) then recursividad b listaSol xs else recursividad b (listaSol++[x]) xs


-- B) Dada la siguiente definici�n de funci�n
doble :: Int -> Int
-- doble x = x + x
-- �C�mo cambiar�a la definici�n utilizando expresiones lambda?
doble = (\x -> x+x)

-- c) Se pide una funci�n en Haskell que dada una lista de n�meros enteros 
-- obtenga un n�mero entero con el resultado de calcular el doble de cada
-- uno de los elementos de la lista original y sumarlos todos. Se piden 
-- diferentes versiones de la misma funci�n:
-- Con recursividad no final
sumaTodoR :: [Int] -> Int
sumaTodoR [] = 0
sumaTodoR (x:xs) = doble x + sumaTodoR xs

-- Con recursividad final o de cola.
sumaTodoRF :: [Int] -> Int
sumaTodoRF = sumaR 0

sumaR :: Int -> [Int] -> Int
sumaR sol [] = sol
sumaR sol (x:xs) = if (x==0) then sumaR sol xs else sumaR (sol+(doble x)) xs

-- Utilizando expresiones lambda u orden superior (se puede hacer uso de 
-- la funci�n predefinida de Haskell map).
sumaTodoLambda :: [Int] -> Int
sumaTodoLambda = foldr (\i sol -> sol+(doble i)) 0

-- D) Implementa una funci�n que sume los cuadrados de los n�meros pares 
-- contenidos en una lista de n�meros enteros. Se piden dos versiones:

-- 1. Una versi�n que haga uso de las funciones de orden superior 
-- de listas map y filter para definir la nueva funci�n.
sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados xs = foldr (+) 0 (map (^2) (filter even xs))

-- 2. Una versi�n que utilice la definici�n de listas por comprensi�n.
sumaCuadradosComprension :: [Int] -> Int
sumaCuadradosComprension xs = foldr (+) 0 [ x*x | x <- xs, even x ]


-- E) Dada una lista de enteros, implementar una funci�n para devolver 
-- tuplas formadas por los elementos (sin repetir) de la lista, junto con la 
-- primera posici�n en la que aparecen.

primeraAparicion :: [Int] -> [(Int, Maybe Int)]
primeraAparicion lista = recursividadA lista

recursividadA :: [Int] -> [(Int, Maybe Int)]
recursividadA [] = [(0, Just 0)]
recursividadA (x:xs) = if (elem x xs) then recursividadA xs else recursividadA xs ++ [(x,(elemIndex x xs))] 

-- F) Implementar en Haskell una funci�n que calcule el n�mero de
--  secuencias de ceros que hay en una lista de n�meros.

ceros :: [Int] -> Int
ceros = cerosAux False 0

cerosAux :: Bool ->  Int -> [Int] ->  Int
cerosAux _ cont [] = cont
cerosAux sec cont (x:xs) = if (x==0) then 
										if (sec==False) then cerosAux True (cont+1) xs else cerosAux sec cont xs
									else 
										if (sec==False) then cerosAux sec cont xs else cerosAux False cont xs

-- G) Implementar una funci�n en Haskell que reciba una lista de n�meros 
-- enteros y devuelva dos listas: una con los elementos sin repetir y otra
-- con los elementos que est�n repetidos.
repeticiones :: [Int] -> ([Int], [Int])
repeticiones = funcLocalRepi ([],[])

funcLocalRepi :: ([Int], [Int]) -> [Int] -> ([Int], [Int])
funcLocalRepi (listaSR, listaR) [] = (listaSR, listaR)
funcLocalRepi (listaSR, listaR) (x:xs) = if (elem x xs) then 
															if  (not(elem x listaR)) then
																funcLocalRepi (listaSR, listaR++[x]) xs
															else
																funcLocalRepi (listaSR, listaR) xs
										 			    else 
										 			    	if  (elem x listaSR || elem x listaR) then
										 			    		funcLocalRepi (listaSR, listaR) xs 
										 					else
										 						funcLocalRepi (listaSR++[x], listaR) xs 
										 				
-- H) Dada una lista de n�meros enteros implementar una funci�n que devuelva 
-- una lista con los n elementos mayores de la lista original.

nmayores :: [Int] -> Int -> [Int]
nmayores lista n = nmayR (reverse (sort lista)) n []

nmayR :: [Int] -> Int -> [Int] -> [Int]
nmayR [] _ sol = reverse sol
nmayR _ 0 sol = reverse sol
nmayR (x:xs) n sol = nmayR xs (n-1) (x:sol)  -- Recursividad Final "Cola"
-- Recursividad NO Final
--nmayR (x:xs) n = x:nmayR xs (n-1)


-- I) Implementa una funci�n incluye en Haskell que reciba dos listas de 
-- n�meros enteros y nos diga si la primera de las listas est� contenida en la 
-- segunda. Se dice que una lista est� contenida en otra si los elementos de la
-- primera aparecen dentro de la segunda, en el mismo orden y de forma consecutiva.

incluye :: [Int] -> [Int] -> Bool
incluye = isInfixOf

-- J) Dada una lista de enteros, se pide implementar una funci�n que ordene dicha 
-- lista de menor a mayor utilizando un algoritmo de inserci�n. Dicho algoritmo 
-- de inserci�n consiste en recorrer la lista L, insertando cada elemento L[i] 
-- en el lugar correcto entre los elementos ya ordenados L[1] ,...,L[i-1].
ordenar :: [Int] -> [Int]
ordenar lista = lista


-- K) Implementa una funci�n polim�rfica en Haskell que reciba 2 listas y vaya
-- cogiendo un elemento de la primera y dos de la segunda, creando una lista 
-- final de ternas. En caso de que una de las dos listas se acabe, mostrar� 
-- la lista de ternas construidas hasta ese momento.

mezclarEnTernas :: [a] -> [b] -> [(a,b,b)]
mezclarEnTernas [] _ = []
mezclarEnTernas _ [] = []
mezclarEnTernas (x:xs) (y1:y2:ys) = [(x,y1,y2)] ++ mezclarEnTernas xs ys
mezclarEnTernas _ _ = []


-- L) Se pide una funci�n polim�rfica en Haskell que dado un elemento y una 
-- lista a�ada dicho elemento al final de la lista.

alFinal :: a -> [a] -> [a]
alFinal x lista = lista ++ [x]


-- M) Mediante la programaci�n de orden superior se pide implementar una de las
-- funciones predefinidas en la librer�a est�ndar de Haskell: la funci�n zipWith.
-- Esta funci�n recibe como par�metros una funci�n y dos listas y une ambas 
-- listas aplicado la funci�n entre los correspondientes par�metros.

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys


-- N) Define una funci�n polim�rfica que sea capaz de invertir los elementos 
-- de una lista. Se piden diferentes versiones:
-- Con recursividad no final
invertirNF :: [a] -> [a]
invertirNF = invRNF

invRNF :: [a] -> [a]
invRNF [] = []
invRNF (x:xs) = invRNF xs ++ [x]

-- Con recursividad de cola o final
invertirF :: [a] -> [a]
invertirF lista = invRF lista []

invRF :: [a] -> [a] -> [a]
invRF [] sol = sol
invRF (x:xs) sol = invRF xs [x]++sol

-- Utilizando la funci�n de orden superior foldr
invertirFOLDR :: [a] -> [a]
invertirFOLDR lista = foldr (\x sol -> sol ++ [x]) [] lista
-- invertirFOLDR [1,2,3,4,5,6]
-- x es [] .... 6
-- ??

-- O) Define una funci�n polim�rfica que sea capaz de invertir los 
-- elementos de una lista de listas.

invertir2 :: [[a]] -> [[a]]
invertir2 [] = []
invertir2 (x:xs) = invertir2 xs ++ [invertirFOLDR x]


-- p) Implementar la funci�n predefinida de la librer�a est�ndar flip. 
-- Esta funci�n lo que hace es recibir una funci�n y devolver otra funci�n
-- que es id�ntica a la funci�n original, salvo que intercambia 
-- los dos primeros par�metros.
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x


-- Q) Implementar la funci�n polim�rfica predefinida de la librer�a est�ndar map. 
-- Esta funci�n lo que hace es recibir una funci�n y una lista y devuelve la 
-- lista resultante de aplicar la funci�n a cada elemento de la lista original.

map2 :: (a -> b) -> [a] -> [b]
map2 f lista = [f x | x <- lista]

map22 ::  (a -> b) -> [a] -> [b]
map22 f [] = []
map22 f (x:xs) = f x : map22 f xs




