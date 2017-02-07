module FuncionesLibro where
import Data.Char;

-- Ejercicio 1. Definir la función media3 tal que (media3 x y z) es
-- la media aritmética de los números x, y y z.
media3 :: Int -> Int -> Int -> Int
media3 x y z = (x+y+z)/3

-- Ejercicio 2. Definir la función sumaMonedas tal que
-- (sumaMonedas a b c d e) es la suma de los euros correspondientes a
-- a monedas de 1 euro, b de 2 euros, c de 5 euros, d 10 euros y
-- e de 20 euros.
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas a b c d e = 1*a+2*b+5*c+10*d+20*e

-- Ejercicio 3. Definir la función volumenEsfera tal que
-- (volumenEsfera r) es el volumen de la esfera de radio r.
--volumenEsfera :: Int -> Int
--volumenEsfera r = (4/3) * pi * r^3

-- Ejercicio 5. Definir la función intercala que reciba dos listas xs e
-- ys de dos elementos cada una, y devuelva una lista de cuatro
-- elementos, construida intercalando los elementos de xs e ys.
intercala :: [a] -> [a] -> [a]
intercala [x1,x2] [y1,y2] = [x1,y1,x2,y2]

-- Ejercicio 6. Definir la función ultimaCifra tal que (ultimaCifra x)
-- es la última cifra del nímero x.
ultimaCifra :: Int -> Int
ultimaCifra x = rem x 10


-- Ejercicio 7. Definir la función rota1 tal que (rota1 xs) es la lista
-- obtenida poniendo el primer elemento de xs al final de la lista.
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]


-- Ejercicio 8. Definir la función rota tal que (rota n xs) es la lista
-- obtenida poniendo los n primeros elementos de xs al final de la
-- lista.
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs


-- Ejercicio 9. Definir la función rango tal que (rango xs) es la
-- lista formada por el menor y mayor elemento de xs.
rango :: Ord a => [a] -> [a]
rango xs = [minimum xs, maximum xs]

-- Ejercicio 10. Definir la función palindromo tal que (palindromo xs) se
-- verifica si xs es un palíndromo; es decir, es lo mismo leer xs de
-- izquierda a derecha que de derecha a izquierda.
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs


-- Ejercicio 11. Definir la función tresIguales tal que
-- (tresIguales x y z) se verifica si los elementos x, y y z son
-- iguales.
tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales x y z = x == y && y == z


-- Ejercicio 12. Definir la función tresDiferentes tal que
-- (tresDiferentes x y z) se verifica si los elementos x, y y z son
-- distintos.
tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes x y z = x /= y && y /= z

-- Ejercicio 13. Definir la función cuatroIguales tal que
-- (cuatroIguales x y z u) se verifica si los elementos x, y, z y u son
-- iguales.
cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales x y z u = x == y && tresIguales y z u

-- Ejercicio 14. Definir la función maxTres tal que (maxTres x y z) es
-- el máximo de x, y y z.
-- f :: f (f x)
maxTres :: Ord a => a -> a -> a -> a
maxTres x y z = max x (max y z)

-- Ejercicio 15. Definir la función divisionSegura tal que
-- (divisionSegura x y) es x/y si y no es cero e y 9999 en caso
-- contrario.
divisionSegura :: Int -> Int -> Maybe Int
divisionSegura _ 0 = Nothing
divisionSegura x y = Just x/y

-- DEFINICION DE FUNCIONES ELEMENTALES

-- Ejercicio 1. Definir la función modulo tal que (modulo v) es el
-- módulo del vector v.
modulo :: (a,a) -> a
modulo (x,y) = sqrt(x^2+y^2)

-- Ejercicio 2. Definir la función cuadrante tal que (cuadrante p) es
-- es cuadrante del punto p (se supone que p no está sobre los
-- ejes). Por ejemplo,
-- cuadrante (3,5) == 1
-- cuadrante (-3,5) == 2
-- cuadrante (-3,-5) == 3
-- cuadrante (3,-5) == 4

cuadrante :: Ord a => (a,a) -> a
cuadrante (x,y)
		| x > 0 && y > 0 = 1
		| x < 0 && y > 0 = 2
		| x < 0 && y < 0 = 3
		| x > 0 && y < 0 = 4


-- Ejercicio 3. Definir la función intercambia tal que (intercambia p)
-- es el punto obtenido intercambiando las coordenadas del punto p.
intercambia :: (a,a) -> (a,a)
intercambia (x,y) = (y,x)


-- 2a soluci�n
raices_2 a b c
			| d >= 0 = [(-b+e)/(2*a), (-b-e)/(2*a)]
			| otherwise = error "No tine raices reales"
				where d = b^2-4*a*c
					  e = sqrt d


-- Ejercicio 6. La disyunción excluyente xor de dos fórmulas se verifica
-- si una es verdadera y la otra es falsa.
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función xor_1 que calcule la disyunción
-- excluyente a partir de la tabla de verdad. Usar 4 ecuaciones, una por
-- cada línea de la tabla.

xor_1 :: Bool -> Bool -> Bool
xor_1 True True = False
xor_1 True False = True
xor_1 False True = True
xor_1 False False = False


-- Ejercicio 6.2. Definir la función xor_2 que calcule la disyunción
-- excluyente a partir de la tabla de verdad y patrones. Usar 2
-- ecuaciones, una por cada valor del primer argumento.

xor_2 :: Bool -> Bool -> Bool
xor_2 True y = not y
xor_2 False y = y


-- Ejercicio 6.3. Definir la función xor_3 que calcule la disyunción
-- excluyente a partir de la disyunción (||), conjunción (&&) y negación
-- (not). Usar 1 ecuación.
xor_3 :: Bool -> Bool -> Bool
xor_3 x y = (x || y) && not (x && y)


Ejercicio 6.4. Definir la función xor_4 que calcule la disyunción
-- excluyente a partir de desigualdad (/=). Usar 1 ecuación.
xor_4 :: Bool -> Bool -> Bool
xor_4 x y = x /= y


-- Ejercicio 7. Definir la función finales tal que (finales n xs) es la
-- lista formada por los n finales elementos de xs.
finales :: a -> [a]
finales n xs = drop (length xs - n) xs

-- Ejercicio 8. Definir la función segmento tal que (segmento m n xs) es
-- la lista de los elementos de xs comprendidos entre las posiciones m y n
segmento :: Int -> Int -> [a]
segmento m n xs = drop (m-1) (take n xs)


-- Ejercicio 9. Definir la función mediano tal que (mediano x y z) es el
-- número mediano de los tres números x, y y z.

mediano x y z = x + y + z - minimum [x,y,z] - maximum [x,y,z]

-- Otra solución es
mediano' x y z
		| a <= x && x <= b = x
		| a <= y && y <= b = y
		| otherwise = z
		where a = minimum [x,y,z]
			  b = maximum [x,y,z]



-- Ejercicio 10. Definir la función distancia tal que (distancia p1 p2)
-- es la distancia entre los puntos p1 y p2.

distancia :: (a,a) -> (a,a) -> Int
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)


-- Ejercicio 11. Definir la función extremos tal que (extremos n xs) es
-- la lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs.
extremos :: Int [a] -> [a]
extremos n xs = take n xs ++ drop (length xs - n) xs



-- Ejercicio 12. Definir la función puntoMedio tal que (puntoMedio p1 p2)
-- es el punto medio entre los puntos p1 y p2.
puntoMedio :: (a,a) -> (a,a) -> (a,a)
puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

-- Page 18






