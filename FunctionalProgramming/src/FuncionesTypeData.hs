-- Util tambien los ALIAS con TYPE, mas facil de recordar.
-- Sinonimos de tipos de datos ya existentes. solo se renombran.
-- type Cadena = [Char]

-- Tipos de Datos Nuevos: Enumerado, Uniones, producto, Recursivos y Polimórficos.
-- Util para definir tipos de datos nuevos con DATA
-- y sus constructores a la derecha:

-- Tipos Enumerado: data Bool = False | True
-- Bool es el constructor de tipo
-- False, True, son constructores de datos

-- Uniones: data LetraOEntero = Letra Char | Entero Int
-- valorCaracter :: LetraOEntero
-- valorCaracter = Letra 'x'
-- valorEntero:: LetraOEntero
-- valorEntero = Entero 23

-- Tipos Producto: 
-- data Persona = P Nombre Int
-- data Persona = P {nombre:: String, edad :: Int}

-- Incluyen constructores de datos simbolicos (binarios). Empieza siempre con " : "
-- data Complejo = Float :- Float
-- data Complejo = :- Float Float

-- Tipos Recursivos: Son utilies para tipos de datos infinitos.
-- data Natural = Cero | Succ Natural
-- data Expr = Valor Int | Expr :+ Expr | Expr :- Expr | Expr :/ Expr | Expr :* Expr

-- Tipos polimórficos: 
-- data Par a = UnPar a a (dos elementos del mismo tipo) 
-- UnPar 1 2
-- data Par a b = UnPar a b (dos elementos que no tienen porque ser del mismo tipo)
-- UnPar True 2

-- Monada para excepciones: Maybe
-- division :: Int -> Int -> Maybe Int
-- division _ 0 = Nothing
-- division m n = Just (div m n)

module FuncionesTypeData where
import Data.List;
import Data.Char;
import Data.String;

-- Operaciones con tuplas
type Persona = (String, Int, Float)
persona :: Persona
persona = ("Rainer", 29, 1.82)

seleccionarPrimero :: Persona -> String
seleccionarPrimero (x,_,_) = x

seleccionarSegundo :: Persona -> Int
seleccionarSegundo (_,y,_) = y

seleccionarTercero :: Persona -> Float
seleccionarTercero (_,_,z) = z


type Nombre = String
type Entero = Int
type Edad = Entero

type Decimales = Float
type Peso = Decimales

-- cada persona, es una tupla con diferentes datos..

juan :: Persona
juan = ("Juan Lopez", 23, 70)
clara :: Persona
clara = ("Clara Fuentes", 35,55)

tocayos :: (Persona,Persona) -> Bool
tocayos((n,_,_),(no,_,_)) = (n == no)

-- Definir una funcion que dado un marcador correspondiente a un partido de 
-- futbol, devuelva el nombre del equipo ganador. Un ejemplo de marcador es:
-- Jaen - Cordoba, 2-2

type Partido = (String, Int, String, Int)
marcador :: Partido 
marcador = ("Jaen", 3, "Cordoba", 2)

--ganador :: marcador -> String
--ganador (nl,gl,nv,gv) = if gl > gv then "Equipo Ganador Local"++Show nl else "Equipo Ganador Visitante"++Show nv


-- Ejercicios sobre el tamano de una matriz de entero.
-- Indicar el numero de filas y columnas.

--Definicion del nuevo Tipo "Matriz".. Una lista de listas de enteros.
-- el constructor es la M seguido del tipo de dato.
type Array = [Entero]
data Matriz = M [Array]
matriz :: Matriz
matriz = M [[1,2,3,4],[9,8,7,6],[5,4,6,7]]

-- Funcion size devulve el numero de filas y columnas
size :: Matriz -> (Int, Int)
size (M []) = (0,0)
size (M matriz) = (length matriz, length (head matriz))


--TIPOS ENUMERADOS :
data Color = Rojo | Verde | Azul | Amarillo deriving Show
red :: Color -- Defino una variable del tipo color
red = Rojo   -- Le asigno un valor de la definicion
-- Nuevo Tipo = Constructores del nuevo tipo.

data Temperatura = Frio | Calor deriving (Show) 
data Estacion = Verano | Otono | Invierno | Primavera deriving (Show)

tiempo :: Estacion -> Temperatura
tiempo Verano = Calor
tiempo Primavera = Calor
tiempo _ = Frio 

temp :: Temperatura -> Estacion
temp Calor = Verano
temp Frio = Invierno 

--Tipos Union..
data LetraOEntero = Letra Char | Entero Int deriving Show

valorCar :: LetraOEntero
valorCar = Letra 'x'
valorEnt :: LetraOEntero
valorEnt = Entero 23


-- Definir una funcion que devuelva en una lista los dias 
-- de la semana que son laborales..
data Dias = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

diasLaborales :: [Dias]
diasLaborales = [Lunes, Martes, Miercoles, Jueves, Viernes]

diasLibres :: [Dias]
diasLibres = [Sabado, Domingo]

-- Dado el tipo Expr visto en clase transparencia 24. 
-- Se pide una funcion que calcule el numero de operaciones
-- que forma parte de una empresion.

-- Se pide una funcion que dada una lista de elementos,
-- te devuelva el menor.
-- CONTEXTO Ord a => Para ordenar y comprar cosas..
-- Poliformico
menor :: Ord a => [a] -> Maybe a
menor [] = Nothing
menor [x] = Just x
menor (x:y:xs) = menor((min x y):xs) 

-- Dividir dos enteros..
dividir :: Int -> Int -> Maybe Int
dividir _ 0 = Nothing
dividir m n = Just (m `div` n)

-- Se pide una funcion que dada una lista de elementos
-- y un elemento, devuelva la posicion del elemento.
posicionIgual :: Ord a => [a] -> a -> Int -> Maybe Int
posicionIgual [] _ _ = Nothing
posicionIgual (x:xs) y c = if x==y then Just 1 else posicionIgual xs y (c+1)
 
-- Se pide una funcion que dado un elemento de la lista de tuplas
-- de dos elementos busque en la primera posicion de la tupla y 
-- devuelva la pareja.
-- CONTENXTO Eq => Solo para comparar el primer elemento!!
parejaTupla :: Eq a => [(a,b)] -> a -> Maybe b
parejaTupla [] _ = Nothing
parejaTupla ((x,y):xs) z = if x==z then Just y else parejaTupla xs z

listaTupla :: [(Int,Int)]
listaTupla = [(1,2),(3,4),(5,6)]


data Person = Pers { nombre :: String,
					 edad :: Int,
					 altura :: Float} deriving Show
					 
					 
					 
-- Tipos de datos recursivos: 
-- Tipos Recursivos
data Natural = Cero | Suc Natural deriving Show
esCero :: Natural -> Bool 
esCero Cero = True 
esCero _ = False 

esPar :: Natural -> Bool 
esPar Cero = True 
esPar (Suc x) = not (esPar x)

-- EJEMPLO: esPar 4 == (Succ(Succ(Succ(Succ(Cero)))))

-- Expresiones aritmeticas:
data Expr = Valor Integer
			|Expr :+: Expr
			|Expr :-: Expr
			|Expr :*: Expr deriving Show

-- Dado el tipo Expr visto en clase, se pide una funcion que calcule el numero 
-- de operaciones que forman parte de una expresion.
ej1 :: Expr
ej1 = Valor 5 -- representa el valor 5
ej2 :: Expr
ej2 = ej1 :+: Valor 3 -- representa el valor 5 + 3
ej3 :: Expr
ej3 = ej2 :*: Valor 10 -- representa el valor (5+3)*10

numOperaciones :: Expr -> Int
numOperaciones (Valor _) = 0
numOperaciones (e1 :+: e2) = 1 + numOperaciones e1 + numOperaciones e2
numOperaciones (e1 :*: e2) = 1 + numOperaciones e1 + numOperaciones e2
-- Consola : numOperaciones (Valor 3 :+: Valor 2 :*: Valor 5)

-- Tipos de datos Poliformicos y Recursivos.
data Par a = UnPar a a deriving Show

parejaEnteros :: Par Int
parejaEnteros = UnPar 1 2 

parejaBool :: Par Bool
parejaBool = UnPar True False

data Pareja a b = P a b deriving Show

-- Crear una funcion que dada una lista de enteros y un numero entero devuelva 
-- la posicion de donde se encuentra.
posicion :: [Int] -> Int -> Maybe Int
posicion lista x = elemIndex x lista
--posicion lista num = posicionAux lista num 0

posicionAux :: [Int] -> Int -> Int -> Maybe Int
posicionAux [] _ _ = Nothing
posicionAUx (x:xs) n pos = if (x==n) then Just pos else posicionAux xs n (pos+1)







