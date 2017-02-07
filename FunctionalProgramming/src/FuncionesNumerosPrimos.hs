module FuncionesNumerosPrimos where 
import Data.List (sort)

-- La criba de Erast�tenes es un m�todo para calcular n�meros primos. 
-- Se comienza escribiendo todos los n�meros desde 2 hasta (supongamos)
-- 100. 
-- El primer n�mero (el 2) es primo. Ahora eliminamos todos los
-- m�ltiplos de 2. El primero de los n�meros restantes (el 3) tambi�n es
-- primo. Ahora eliminamos todos los m�ltiplos de 3. El primero de los
-- n�meros restantes (el 5) tambi�n es primo ... y as�
-- sucesivamente. Cuando no quedan n�meros, se han encontrado todos los
-- n�meros primos en el rango fijado.

listaNum :: [Int]
listaNum = [1000000..2000000]

listaMod :: [Int]
listaMod = [2..1000000]

f :: [Int] -> Int
f xs = last xs

-- Criba de numeros de la lista por comprensi�n:
elimina :: Int -> [Int] -> [Int]
elimina n [] = []
elimina n xs = [ x | x <- xs, x `mod` n /= 0 ]
						
criba :: [Int] -> [Int]
criba [] = []
criba (n:ns) = n : criba (elimina n ns)

-- Criba de numeros de la lista por recursi�n:
cribaR :: Int -> [Int] -> [Int]
cribaR n [] = []
cribaR n (x:xs) 
				| mod x n == 0 = cribaR n xs
				| otherwise = x : cribaR n xs

-- Criba de numeros de la lista por plegado:
cribaP :: Int -> [Int] -> [Int]
cribaP n = foldr f [] 
		   where f x y 
		   			   | mod x n == 0 = y
			           | otherwise = x:y
			           		           