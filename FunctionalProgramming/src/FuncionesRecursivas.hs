module FuncionesRecursivas where
								  

-- Definir una funcion que calcule la longitud de una lista.
-- Se piden tres versiones de la funcion:
-- 1) Definir la funcion con una unica ecuacion.
-- 2) Definir la funcion con varias ecuaciones con ajuste de patrones.
-- 3) Definir la funcion utilizando la expresion condicional CASE

longitudLista1 :: [Int] -> Int
longitudLista1 xs = if xs==[] then 0 else 1+longitudLista1(tail xs)

longitudLista2 :: [a] -> Int
longitudLista2 [] = 0
longitudLista2 xs = 1 + longitudLista2(tail xs)

longitudLista3 :: [a] -> Int
longitudLista3 xs = case xs of
								[]     -> 0
								(_:xs) -> 1 + longitudLista3 xs
						
-- Cuenta el numero de caracteres de una cadena.
-- RECURSIVIDAD LINEAL: NO FINAL!!						
cuentaChar :: String -> Int
cuentaChar [] = 0
cuentaChar (x:xs) = 1 + cuentaChar xs


-- Cuenta el numero de caracteres de una cadena del 
-- caracter indicado en la cadena especificada.
-- RECURSIVIDAD LINEAL: NO FINAL!!
cuentaCharC :: String -> Char -> Int
cuentaCharC [] _ = 0
cuentaCharC (x:xs) c = if x==c then 
									1 + cuentaCharC xs c
								else 
									cuentaCharC xs c

-- Mismo ejercicio pero con RECURSIVIDAD LINEAL: FINAL(COLA)!!
cuentaCharCFinal :: String -> Char -> Int
cuentaCharCFinal xs c = countCharFinal(xs, c, 0)

countCharFinal :: (String, Char, Int) -> Int
countCharFinal([], _, cont) = cont
countCharFinal((x:xs), c, cont) = if x==c then 
												countCharFinal(xs, c, cont+1) 
											else  
												countCharFinal(xs, c, cont)
 
									
-- Factorial de un Numero. RECURSIVIDAD LINEAL NO FINAL!!
factorial :: Int -> Int
factorial x = if x == 0 then 1 else x*factorial(x-1)
-- Con PATRONES:
-- factoria 0 = 1
-- factoria x = x*factorial(x-1)

-- Factorial de un Numero. RECURSIVIDAD LINEAL: FINAL(COLA)!!
fact :: Int -> Int
fact n = fact2(n,1)

fact2 :: (Int, Int) -> Int
fact2 (n,r) = if (n==1) then r else fact2(n-1, r*n)  


-- Definir una funcion recursiva que dada una lista de numeros enteros devuelva
-- como resultado la suma de todos ellos.
-- 1) Implementacion con Recursividad NO FINAL.
-- 2) Implementacion con Recursividad FINAL.

sumaNOFINAL :: [Int] -> Int
sumaNOFINAL [] = 0
sumaNOFINAL (x:xs) = x + sumaNOFINAL xs

sumaFinal :: [Int] -> Int
sumaFinal xs = sumaF xs 0

sumaF :: [Int] -> Int -> Int
sumaF [] cont = cont
sumaF (x:xs) cont = sumaF xs (cont+x) 

