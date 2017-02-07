module EjerciciosHoja1 where
import Data.Char;

-- A) Implementar una funci�n en Haskell que dados 
-- tres n�meros enteros determine si est�n ordenados 
-- de menor a mayor.

ordenados2Bool :: (Int, Int) -> Bool 
ordenados2Bool (x, y) = if (x <= y) then True else False

ordenados3Bool :: (Int, Int, Int) -> Bool 
ordenados3Bool (x, y, z) = if (ordenados2Bool(x, y) && ordenados2Bool (y, z)) then True else False
			
-- B) Implementar una funci�n en Haskell que dados 
-- tres n�meros enteros los devuelva ordenados de menor a mayor.

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

-- C) Crear una funci�n que reciba el radio de una 
-- circunferencia y devuelva una 2-tupla con la longitud 
-- de la circunferencia y con el �rea del c�rculo. 
-- Emplea una definici�n local con la cl�usula where para 
-- almacenar el valor de Pi. A continuaci�n crear una funci�n 
-- con el mismo cometido empleando la definici�n local let.

longAreaCircuLet :: Float -> (Float, Float)
longAreaCircuLet r = let pi2 = pi in (2 * pi2 * r, pi2 * r^2)

longAreaCircuWhere :: Float -> (Float, Float)
longAreaCircuWhere r = (2 * pi2 * r, pi2 * r^2 ) where pi2 = pi


-- D) Implementar la funci�n predefinida de listas concat, 
-- que se llamar� concatenar, utilizando la definici�n de 
-- listas por comprensi�n (no se puede utilizar recursividad).
concatenar :: ([a],[a]) -> [a]
concatenar ([],xs) = xs
concatenar (xs,[]) = xs
concatenar (xs,ys) = xs ++ ys

-- E) Implementar una funci�n que dado un n�mero entero 
-- devuelva en una lista todos los factores de dicho n�mero. 
-- Se debe utilizar la definici�n de listas por comprensi�n.
-- En matem�ticas, los factores de un n�mero son los n�meros 
-- enteros que pueden multiplicarse juntos para igualar ese 
-- n�mero. O tambi�n se puede decir que los factores de un 
-- n�mero son n�meros enteros por el que un n�mero es divisible.
factores :: Int -> [Int]
factores x = [y | y <- [1..x], mod x y == 0]

-- F) Implementar una funci�n que diga si un n�mero es primo. 
-- Para ello se debe utilizar la funci�n que calcula el n�mero 
-- de factores de un n�mero (ejercicio f).
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo x = ((length (factores x)) <= 2)

-- la funcion primos, no recibe nada y devulve una lista de enteros
primos :: [Int]
primos = criba [2..]
  		 where
    		criba (p:xs) = p:criba [x | x <- xs, mod x p > 0]
    

-- G) Implementar una funci�n que diga cu�ntos caracteres en 
-- may�scula est�n contenidos en una frase dada. Se deber� 
-- utilizar la definici�n de listas por comprensi�n.
count :: String -> String
count xs = [x | x <- xs, isUpper x == True]
 
charCont :: String -> Int
charCont [] = 0
charCont (xs) = length (count (xs))


-- H) Implementar una funci�n que dada una tupla de tres elementos,
-- donde cada uno de ellos es a su vez una tupla de dos elementos
-- de tipo String e Int respectivamente, retorne el primer
-- elemento de cada tupla interna. Se deber� utilizar ajuste de patrones.
primerElemTuplaTupla :: ((String, Int), (String, Int), (String, Int)) -> [String]
primerElemTuplaTupla (([],_), ([],_), ([],_)) = []
primerElemTuplaTupla ((xs,_), ([],_), ([],_)) = [xs]
primerElemTuplaTupla (([],_), (ys,_), ([],_)) = [ys]
primerElemTuplaTupla (([],_), ([],_), (zs,_)) = [zs]
primerElemTuplaTupla (([],_), (ys,_), (zs,_)) = [ys ++ zs]
primerElemTuplaTupla ((xs,_), ([],_), (zs,_)) = [xs ++ zs]
primerElemTuplaTupla ((xs,_), (ys,_), ([],_)) = [xs ++ ys]
primerElemTuplaTupla ((xs,_), (ys,_), (zs,_)) = [xs ++ ys ++ zs]


-- I) Implementar una funci�n que devuelve True si la suma de los cuatro 
-- primeros elementos de una lista de n�meros enteros es un valor menor a 10
-- y devolver� False en caso contrario. Se deber� utilizar ajuste de patrones.

suma4List :: [Int] -> Bool
suma4List (c1:c2:c3:c4:cs) = if (c1+c2+c3+c4) < 10 then True else False
suma4List (_) = False

-- J) Implementar una funci�n que dado un car�cter, que representa un punto 
-- cardinal, devuelva su descripci�n. Por ejemplo, dado �N� devuelva �Norte�.

cardinal :: Char -> String
cardinal x 
			| (x == 'N' || x == 'n') = "Norte"
			| (x == 'S' || x == 's') = "Sur"
			| (x == 'E' || x == 'e') = "Este"
			| (x == 'O' || x == 'o') = "Oeste"


-- K) Implementar una funci�n que dada una frase retorne un mensaje donde
--  se indique cu�l es la primera y �ltima letra de la frase original.
primeraUltimaChar :: String -> String
primeraUltimaChar [] = "No hay Frase"
primeraUltimaChar (x:[])  = [x] ++ [x]
primeraUltimaChar (x:xs)  = x:[last xs]


-- L) Implementar una funci�n que dado un n�mero entero devuelva mensajes 
-- indicando en qu� rango de valores se encuentra dicho n�mero 
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
						


-- M) Implementar una funci�n que dada una cadena de caracteres y un car�cter, 
-- indique el n�mero de apariciones del car�cter en la cadena. No se debe 
-- utilizar recursividad, s� ajuste de patrones. Pista: utilizar la definici�n 
-- de listas por comprensi�n.
contar :: String -> Char -> String
contar xs c = [x | x <- xs, x == c]

contarApariciones :: String -> Char -> Int
contarApariciones [] _ = 0
contarApariciones xs c = length (contar xs c)