module FuncionesNumerosPrimos where 
import Data.List (sort)

-- La criba de Erastótenes es un método para calcular números primos. 
-- Se comienza escribiendo todos los números desde 2 hasta (supongamos)
-- 100. 
-- El primer número (el 2) es primo. Ahora eliminamos todos los
-- múltiplos de 2. El primero de los números restantes (el 3) también es
-- primo. Ahora eliminamos todos los múltiplos de 3. El primero de los
-- números restantes (el 5) también es primo ... y así
-- sucesivamente. Cuando no quedan números, se han encontrado todos los
-- números primos en el rango fijado.

listaNum :: [Int]
listaNum = [1000000..2000000]

listaMod :: [Int]
listaMod = [2..1000000]

f :: [Int] -> Int
f xs = last xs

-- Criba de numeros de la lista por comprensión:
elimina :: Int -> [Int] -> [Int]
elimina n [] = []
elimina n xs = [ x | x <- xs, x `mod` n /= 0 ]
						
criba :: [Int] -> [Int]
criba [] = []
criba (n:ns) = n : criba (elimina n ns)

-- Criba de numeros de la lista por recursión:
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
			           		           