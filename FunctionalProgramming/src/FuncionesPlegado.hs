module FuncionesDeplegado where

-- PREGUNTAR A SOTO-------------
dosVeces :: (Int -> Int) -> Int -> Int
dosVeces f x = f (f x) 

sumac:: Int-> (Int-> Int)
sumac x y = x + y
---------------------------------
-- Función de plegado de listas por la derecha: foldr
-- FOLDR recibe como parámetros un operador(+), un valor inicial e y una lista[Char].
-- foldr (+) e [w,x,y,z] ==> (w + (x + (y + (z + e))))
-- APLICACION PARCIAL PORQUE NO ESPECIFICA LA LISTA xs en la ecuacion.

sumaLista :: [Int] -> Int
sumaLista = foldr (+) 0
-- (1 + (2 + (3 + 0)))
-- sumaLista [1,2,3] ==> 6

verdad :: [Bool] -> Bool
verdad = foldr (&&) True
-- (True && (False && (True && True)))
--verdad [1<2, False, 3*4<20] ==> False

concatenar :: [[Int]] -> [Int]
concatenar = foldr (++) []
-- [[1,2],[3],[4,5,6]] 
-- ([1,2] ++ ([3] ++ ([4,5,6] ++ [])) ==> [1,2,3,4,5,6]

-- FOLDL : Lo mismo pero empieza por la izquierda de los argumentos.

-- FUNCIONES ANONIMAS. LAMBDA. Son utiles cuando se esta trabajando con 
-- funciones de orden superior. Se utiliza cuando se utilizan una vez en 
-- casos concretos y no es necesario declararlas.

-- 5 'mod' 2 --Notacion infija
-- mod 5 2  --Notacion prefija

invertirLista :: [Int] -> [Int]
invertirLista = foldr (\i lista -> lista ++ [i]) []
--EJECUCION: [1,2,3]
-- [3] ++ []
-- [2] ++ [3]
-- [1] ++ [3,2]
-- resultado: [3,2,1]

-- EJERCICIO) Definir una funcion que calcule la longitud de una lista de 
-- numeros enteros utilizando la funcion de plegado por la derecha y 
-- funciones anonimas.
longAnon :: [Int] -> Int
longAnon = foldr (\_ n -> n+1) 0
--EJECUCION: [1,2,3]
-- 3 (0+1) 0
-- 2 (1+1) 1
-- 1 (2+1) 2
-- resultdo:3

-- f = foldr (\a b->[a]:b) []
--EJECUCION: [1,2,3]
-- [3] (\a b->[a]:b) []
-- [2] (\a b->[a]:b) [[3]]
-- [1] (\a b->[a]:b) [[2],[3]]
-- resultado [[1],[2],[3]]

-- f = foldl (\a b -> if b==2 then 0:a else b:a) []
--EJECUCION: [1,2,3]
-- [] (\a b -> if b==2 then 0:a else b:a) 1
-- [1] (\a b -> if b==2 then 0:a else b:a) 2
-- [0,1] (\a b -> if b==2 then 0:a else b:a) 3
-- resultado [3,0,1]

-- Definir una funcion que reciba una lista de funciones que se aplican a un 
-- segundo argumento de tipo entero y retorna una lista de enteros con el resultado
-- de aplicar cada funcion al segundo argumento.
-- funcion :: ([f] -> Int) -> [Int]
funcion :: [Int -> Int] -> Int -> [Int]
funcion [] _ = []
funcion (f:fs) x = f x : funcion fs x

--Se aplica la misma funcion: multiplicar a cada uno de los elementos.
--funcion :: [Int] -> Int -> [Int]
--funcion xs b = map (*b) xs

--POLIMORFISMO:
dosVecesPol :: (a -> a) -> a -> a
dosVecesPol f x = f (f x)


-- Definir una funcion que dadas dos listas de tuplas de dos elementos de 
-- cualquier tipo, retorne una lista de tuplas de tuplas combinando las dos tuplas
--mezclar :: [(a,b)] -> [(c,d)] -> [((a,c),(b,d))]
--mezclar [] _ = []
--mezclar _ [] = []
--mezclar ((x1,x2:xs)) ((y1,y2:ys)) = mezclar xs ys:[((x1,y1),(x2,y2))]
