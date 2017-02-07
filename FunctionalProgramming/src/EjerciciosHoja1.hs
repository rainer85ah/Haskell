module EjerciciosHoja1 where
import Data.Char;

-- A) Implementar una función en Haskell que dados 
-- tres números enteros determine si están ordenados 
-- de menor a mayor.

ordenados2Bool :: (Int, Int) -> Bool 
ordenados2Bool (x, y) = if (x <= y) then True else False

ordenados3Bool :: (Int, Int, Int) -> Bool 
ordenados3Bool (x, y, z) = if (ordenados2Bool(x, y) && ordenados2Bool (y, z)) then True else False
			
-- B) Implementar una función en Haskell que dados 
-- tres números enteros los devuelva ordenados de menor a mayor.

ordenados2Asc :: (Int, Int) -> (Int, Int)
ordenados2Asc (x, y) = if ordenados2Bool(x,y) then (x, y) else (y, x)

ordenados3Asc :: (Int, Int, Int) -> (Int, Int, Int)
ordenados3Asc (x, y, z) 
						| (x <= y && y <= z) = (x, y, z) 
						| (x <= z && z <= y) = (x, z, y) 
						| (y <= x && x <= z) = (y, x, z)
						| (y <= z && z <= x) = (y, z, x)
						| (z <= x && x <= y) = (z, x, y)
						| (z <= y && y <= x) = (z, y, x)

-- C) Crear una función que reciba el radio de una 
-- circunferencia y devuelva una 2-tupla con la longitud 
-- de la circunferencia y con el área del círculo. 
-- Emplea una definición local con la cláusula where para 
-- almacenar el valor de Pi. A continuación crear una función 
-- con el mismo cometido empleando la definición local let.

longAreaCircuLet :: Float -> (Float, Float)
longAreaCircuLet r = let pi2 = pi in (2 * pi2 * r, pi2 * r^2)

longAreaCircuWhere :: Float -> (Float, Float)
longAreaCircuWhere r = (2 * pi2 * r, pi2 * r^2 ) where pi2 = pi


-- D) Implementar la función predefinida de listas concat, 
-- que se llamará concatenar, utilizando la definición de 
-- listas por comprensión (no se puede utilizar recursividad).
concatenar :: ([a],[a]) -> [a]
concatenar ([],xs) = xs
concatenar (xs,[]) = xs
concatenar (xs,ys) = xs ++ ys

-- E) Implementar una función que dado un número entero 
-- devuelva en una lista todos los factores de dicho número. 
-- Se debe utilizar la definición de listas por comprensión.
-- En matemáticas, los factores de un número son los números 
-- enteros que pueden multiplicarse juntos para igualar ese 
-- número. O también se puede decir que los factores de un 
-- número son números enteros por el que un número es divisible.
factores :: Int -> [Int]
factores x = [y | y <- [1..x], mod x y == 0]

-- F) Implementar una función que diga si un número es primo. 
-- Para ello se debe utilizar la función que calcula el número 
-- de factores de un número (ejercicio f).
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo x = ((length (factores x)) <= 2)

-- la funcion primos, no recibe nada y devulve una lista de enteros
primos :: [Int]
primos = criba [2..]
  		 where
    		criba (p:xs) = p:criba [x | x <- xs, mod x p > 0]
    

-- G) Implementar una función que diga cuántos caracteres en 
-- mayúscula están contenidos en una frase dada. Se deberá 
-- utilizar la definición de listas por comprensión.
count :: String -> String
count xs = [x | x <- xs, isUpper x == True]
 
charCont :: String -> Int
charCont [] = 0
charCont (xs) = length (count (xs))


-- H) Implementar una función que dada una tupla de tres elementos,
-- donde cada uno de ellos es a su vez una tupla de dos elementos
-- de tipo String e Int respectivamente, retorne el primer
-- elemento de cada tupla interna. Se deberá utilizar ajuste de patrones.
primerElemTuplaTupla :: ((String, Int), (String, Int), (String, Int)) -> [String]
primerElemTuplaTupla (([],_), ([],_), ([],_)) = []
primerElemTuplaTupla ((xs,_), ([],_), ([],_)) = [xs]
primerElemTuplaTupla (([],_), (ys,_), ([],_)) = [ys]
primerElemTuplaTupla (([],_), ([],_), (zs,_)) = [zs]
primerElemTuplaTupla (([],_), (ys,_), (zs,_)) = [ys ++ zs]
primerElemTuplaTupla ((xs,_), ([],_), (zs,_)) = [xs ++ zs]
primerElemTuplaTupla ((xs,_), (ys,_), ([],_)) = [xs ++ ys]
primerElemTuplaTupla ((xs,_), (ys,_), (zs,_)) = [xs ++ ys ++ zs]


-- I) Implementar una función que devuelve True si la suma de los cuatro 
-- primeros elementos de una lista de números enteros es un valor menor a 10
-- y devolverá False en caso contrario. Se deberá utilizar ajuste de patrones.

suma4List :: [Int] -> Bool
suma4List (c1:c2:c3:c4:cs) = if (c1+c2+c3+c4) < 10 then True else False
suma4List (_) = False

-- J) Implementar una función que dado un carácter, que representa un punto 
-- cardinal, devuelva su descripción. Por ejemplo, dado ‘N’ devuelva “Norte”.

cardinal :: Char -> String
cardinal x 
			| (x == 'N' || x == 'n') = "Norte"
			| (x == 'S' || x == 's') = "Sur"
			| (x == 'E' || x == 'e') = "Este"
			| (x == 'O' || x == 'o') = "Oeste"


-- K) Implementar una función que dada una frase retorne un mensaje donde
--  se indique cuál es la primera y última letra de la frase original.
primeraUltimaChar :: String -> String
primeraUltimaChar [] = "No hay Frase"
primeraUltimaChar (x:[])  = [x] ++ [x]
primeraUltimaChar (x:xs)  = x:[last xs]


-- L) Implementar una función que dado un número entero devuelva mensajes 
-- indicando en qué rango de valores se encuentra dicho número 
-- (menor de 10, entre 10 y 20 o mayor de 20). Se debe utilizar definiciones locales.

funcionLocal :: Int -> String
funcionLocal x 
				| x == 0 = "menor de 10"
				| x == 1 = "entre 10 y 20"
				| x == 2 = "mayor de 20"

rangoMensaje :: Int -> String
rangoMensaje x = if x < 10 then funcionLocal 0 else
					if x >= 10 && x <= 20 then funcionLocal 1 else
						if x > 20 then funcionLocal 2 else "Numero Negativo"
						


-- M) Implementar una función que dada una cadena de caracteres y un carácter, 
-- indique el número de apariciones del carácter en la cadena. No se debe 
-- utilizar recursividad, sí ajuste de patrones. Pista: utilizar la definición 
-- de listas por comprensión.
contar :: String -> Char -> String
contar xs c = [x | x <- xs, x == c]

contarApariciones :: String -> Char -> Int
contarApariciones [] _ = 0
contarApariciones xs c = length (contar xs c)