module FunctionesSimples where
import Data.Char;
 
incremento :: Int -> Int
incremento x = x + 1

decremento :: Int -> Int
decremento x = x - 1

celsius :: Int -> Int
celsius f = (f - 32)*5 `div` 9

multiploDe :: Int -> Int -> Bool
multiploDe p n = mod n p == 0

esPar :: Int -> Bool
esPar = multiploDe 2

suma :: Int -> Int -> Int
suma x y = x + y

succesor :: Int -> Int
succesor = suma 1

doble :: Int -> Int
doble x = x + x

--Composicion: es lo mismo que f(g(x)) = (HASKELL) f.g --
cuadruple :: Int -> Int
cuadruple = doble.doble 

-- bien: cuadruple x = doble(doble x) --Composicion Normal.
-- mal: cuadruple x = doble.(doble x)-- Porque despues de un punto debe ser funciones a la izq y derecha.
esImpar :: Int -> Bool
esImpar = not.esPar

--La funcion not esta ya predefinida en HASKELL: not:: Bool->Bool --
-- import Data.Char; Contiene funciones para el tipo de datos char.
-- isUpper,isDigit,toLower


-- TUPLAS son con PARENTISIS 
f :: (Int, Int) -> (Int, Int)
f (m, n) = (m+n, m-n)

-- Condicion IF, ELSE OBLIGADAO
abs :: Int -> Int
abs n = if n >= 0 then n else negate n

max :: (Int, Int) -> Int
max (m, n) = if m >= n then m else n

--Condicion CASE
paridad :: Int -> Bool
paridad x = case (mod x 2) of
								0 -> True
								1 -> False
				
--Condicion GUARDAS: | exp a la izq (siempre BOOL) = resultado a la dcha. 	
mayor2 :: (Int , Int) -> Int
mayor2 (x, y)
			| x > y = x		
			| otherwise = y

mayor3 :: (Int, Int, Int) -> Int
mayor3 (x, y, z) = mayor2(mayor2(x, y), mayor2(y, z))


-- Definir una funcion que dados dos intervalos de numeros enteros diga
-- si hay interseccion entre ellos.
interseccion :: (Int, Int) -> (Int, Int) -> Bool
interseccion (a, b) (x, y) 
							| b > x && a < y = True 
							| y > b && x < a = True 
							| otherwise = False
							
-- Definir una funcion que calcule el maximo de tres numeros con guardas.
max3 :: (Int, Int, Int) -> Int
max3 (x, y, z)
				| x >= y && x >= z = x
				| y >= x && y >= z = y
				| z >= y && z >= x = z


mayor :: Int -> Int -> Int
mayor x y = if x >= y then x else y

suma2 :: Int -> Int -> Int
suma2 x y = x+y

-- Devuelve dos caracteres ordenados en una tupla.
ordenarChar :: (Char, Char) -> (Char, Char)
ordenarChar (x, y) = if y >= x then (x, y) else (y, x)

-- Devuelve el area de la circunferencia
areaCirculo :: Float -> Float
areaCirculo r = 2*pi*r
 
-- Devuleve el iva de una cantidad
iva :: Float -> Float
iva x = x * 0.21

-- Hacerlo con let..
ivaLet :: Float -> Float
ivaLet x =
	let y = 1.21 --Definiciones
	in x*y		 --Expresiones

--Hacerlo con where..
ivaWhere :: Float -> Float
ivaWhere x = y 
    where y = x * 1.21 -- Expresion
    

-- Factorial de un numero: Recursividad Lineal - NO Final
-- Con definicion de patrones..
fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

-- Factorial de un numero: Recursividad Lineal - Final
-- Una segunda funcion privada.
factorial :: Int -> Int
factorial x = fact2(x,1)

fact2 :: (Int,Int) -> Int
fact2 (x,y) = if x == 1 then y else fact2(x-1,x*y)


-- PARCIAL : Notacion Currificada..
suma3 :: Int -> Int -> Int -> Int
suma3 x y = suma2 x.suma2 y

 

-- Devuelve si dos rangos(intervalos) si uno esta dentro del otro.
comunRangos :: ((Int, Int), (Int, Int)) -> Bool
comunRangos ((x,y),(a,b)) 
                | (x > a) && (y < b) = True
                | (x > a) && (y > b) = True
                | (a > x) && (b < y) = True
                | (a > x) && (b > y) = True
                | otherwise = False
                
	