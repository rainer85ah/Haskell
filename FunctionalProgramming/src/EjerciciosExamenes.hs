module EjerciciosExamenes where
import Data.List;

{-
1) Dada una matriz cuadrada expresada en Haskell como [[Int]], se pretende 
desarrollar un código Haskell que compruebe si todas las filas y todas las 
columnas de la matriz suman lo mismo. Para ello, se parte de un código ya 
implementado que contiene algunos errores.
-}
matriz :: [[Int]] -> Bool
matriz [] = True
matriz (x:xs) = horizontales (suma(x),x:xs) && verticales(suma(x),x:xs)

horizontales :: (Int, [[Int]]) -> Bool
horizontales (_,[]) = True
horizontales (valor, x:xs) = if suma(x) == valor then horizontales(valor,xs) else False

verticales :: (Int, [[Int]]) -> Bool
verticales (_,[]) = True
verticales (valor, m) = if suma(dameColumna(m))/=0 then verticales(valor,quitaColumna(m)) else False

dameColumna :: [[Int]] -> [Int]
dameColumna [] = []
dameColumna ((y:ys):xs) = y:dameColumna(xs)

quitaColumna :: [[Int]] -> [[Int]]
quitaColumna [] = []
quitaColumna (x:xs) = x:quitaColumna(xs)

suma :: [Int] -> Int
suma (x:xs) = x + suma(xs)
   

{- 
2) Se pide implementar un programa en Haskell que sea capaz de obtener el 
número de errores de una tabla de multiplicar, junto con los elementos de la 
tabla con los errores.
Por ejemplo, si recibe la siguiente tabla del 1 el resultado debería de ser 
algo como: "Hay 0 errores, que son: ".
-}
type Fila = (Int, Int, Int)
type ContError = Int
type Tabla = [Fila]
data TablaResultado = TR ContError [Fila] deriving (Eq, Ord)

-- La idea es crear una funcion que genere una tabla de multiplicar del uno y otra funcion que compara dos tablas.
tablaMult :: Int -> Tabla
tablaMult num = [(num, n, num*n) | n <- [1..10]]

tabla1 :: Tabla
tabla1 = tablaMult 1

compararTablas :: TablaResultado -> Tabla -> Tabla -> TablaResultado
compararTablas sol [] [] = sol
compararTablas sol _ []  = sol
compararTablas sol [] _  = sol
compararTablas (TR c lista) ((x1,x2,x3):xs) ((y1,y2,y3):ys) = if (x1==y1 && x2==y2 && x3==y3) then 
																					compararTablas (TR c lista) xs ys 
																				 else 
																				 	compararTablas (TR (c+1) (lista++[(x1,x2,x3)])) xs ys

numeroErrores :: Tabla -> Tabla -> TablaResultado
numeroErrores t1 t2 = (compararTablas (TR 0 []) t1 t2)

instance Show TablaResultado where
	show (TR num lista) = "Hay " ++ show num ++ " errores, que son: " ++ show lista ++ "."


{-
4) Se pide implementar un programa en Haskell que sea capaz de obtener las 
estadísticas de los resultados de un equipo de fútbol en una temporada determinada. 
Debe ser lo suficientemente general como para que se pueda aplicar a diferentes 
temporadas y en diferentes ligas.
Una temporada se compone de diferentes jornadas, cada jornada se compone de una 
serie de encuentros y cada encuentro contiene información de los dos equipos 
que y del resultado final del partido. 
-}

data Liga = BBVA | Champpion | UEFA | CopaDelRey deriving (Show, Eq, Ord)
type Equipo = String
type Goles = Int
type Partido = (Equipo, Equipo, Goles, Goles)
type Jornada = [Partido]
type NombreTemp =String
data Temporada = T NombreTemp [Jornada] Liga deriving (Show, Ord, Eq)
data DDBB = DB [Temporada] deriving (Show, Ord, Eq)
data Resultado = R (Int, Int, Int) deriving (Ord, Eq)

estadisticas :: DDBB -> Equipo -> NombreTemp -> Liga -> Resultado
estadisticas = estaAux (R (0, 0, 0))

estaAux :: Resultado -> DDBB -> Equipo -> NombreTemp -> Liga -> Resultado
estaAux (R result) _ "" _ _ = (R result)
estaAux (R result) _ _ "" _ = (R result)
estaAux (R result) (DB db) nEquipo nTemp liga = counter(nEquipo, (devolverJornadas (DB db) nTemp liga), (R result))
																						
devolverJornadas :: DDBB -> NombreTemp -> Liga -> [Jornada]
devolverJornadas (DB []) _ _ = []
devolverJornadas _ "" _ = []
devolverJornadas (DB ((T nT jornadas league):db)) nombTemp liga = if (nombTemp==nT && liga==league) then jornadas
						
counter :: (nEquipo, [Jornada], Resultado) -> Resultado
counter ("", _, (R result)) = (R result)
counter (_, [], (R result)) = (R result)
counter (nombre, (((e1,e2,g1,g2):jss), (R (g,e,p))) = if (nombre==e1) then 
																		if (g1==g2) then 
																					counter (nombre, jss, (R (g,e+1,p)))
																		else 
																			if (g1>g2) then 
																						counter (nombre, jss, (R (g+1,e,p)))
																      	   			else 
																					 	counter (nombre, jss, (R (g,e,p+1)))
																					 	
																	  else if (nombre==e2) then 
																				if (g1==g2) then 
																								counter (nombre, jss, (R (g,e+1,p)))
																						   else 
																						   		if (g1<g2) then 
																									counter (nombre, jss, (R (g+1,e,p)))
																								else 
																									counter (nombre, jss, (R (g,e,p+1)))
																		else 
																			counter (nombre, jss, (R (g,e,p)))
																				
t1 :: Temporada
t1 = T "Temporada2015-2016" [j1,j2,j3] BBVA
db :: DDBB
db = DB [t1]
 
instance Show Resultado where
		show (R (a, b, c)) = "(Ganados: " ++ show a ++ " , Empatados: " ++ show b ++ ", Perdidos: " ++ show c ++ ")"

-- Jornada 1:
p1 :: Partido
p1 = ("R. Madrid", "Betis", 2, 1)
p2 :: Partido
p2 = ("Atletico de Madrid", "Valencia", 6, 1)
j1 :: Jornada
j1 = [p1,p2]

--Jornada 2:
p3 :: Partido
p3 = ("Valencia", "R. Madrid", 1, 5)
p4 :: Partido
p4 = ("Betis", "Atletico de Madrid", 3, 3)
j2 :: Jornada
j2 = [p3,p4]

--Jornada 3:
p5 = ("Betis", "Valencia", 0, 0)
p6 = ("R. Madrid", "Atletico de Madrid", 3, 3)
j3 :: Jornada
j3 = [p5,p6]


{-
6) La criba de Eratóstenes es un algoritmo que permite hallar todos los números 
primos menores que un número natural dado n. Se parte de una lista con todos los 
números naturales comprendidos entre 2 y n, y se van eliminando los números que
 no son primos de la siguiente manera: Comenzando por el 2, se eliminan todos 
 sus múltiplos; comenzando de nuevo, cuando se encuentra un número entero que 
 no ha sido eliminado, ese número es declarado primo, y se procede a eliminar 
 todos sus múltiplos, así sucesivamente.
-}
-- Criba de numeros de la lista por comprensión:
listaNum :: [Int]
listaNum = [2..1000]

listaMod :: [Int]
listaMod = [2..100]

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
			           

{-
7) Dada la siguiente definición de tipos:Dada la siguiente definición de tipos:
data Arbol a = AV | Rama (Arbol a) (a) (Arbol a) deriving Show
Implementa la operacion recorridoPreOrden, que recibe un arbol a y devuelve en una
 lista los valores de a almacenados en el arbol a siguiendo dicho recorrido.
-}
data Arbol a = AV | Rama (Arbol a) (a) (Arbol a) deriving (Show, Eq)

preOrder :: Arbol a -> [a]
preOrder AV = []
preOrder (Rama izq r der) = [r] ++ preOrder izq ++ preOrder der


{-
8) Implementa la funcion hermanos, que recibe un Arbol a y dos nodos y que devuelva 
True si esos dos nodos tienen el mismo padre y False en caso contrario.
-}
sonHermanos :: (Eq a) => Arbol a -> a -> a -> Bool
sonHermanos (Rama izq r der) n1 n2 = ( (padre izq n1) == (padre izq n2) || (padre der n1) == (padre der n2) )
sonHermanos _ _ _ = False

padre :: (Eq a) => Arbol a -> a -> a
padre AV _ = error "Arbol Vacio!"
padre (Rama izq r der) n 
						| ((raiz izq)==n || (raiz der)==n) = r 
						| ((raiz izq)/=n) = padre izq n
						| ((raiz der)/=n) = padre der n

raiz :: Arbol a -> a
raiz AV = error "Arbol Vacio!"
raiz (Rama _ r _) = r

												  	  
{-
9) A partir de la información sobre los torneos de tenis celebrados en una 
temporada, donde de cada torneo se conoce el nombre del mismo, el nombre de los 
dos jugadores finalistas, y el resultado del partido, tal y como se puede ver 
en el ejemplo de la Tabla 1.

Se pide implementar el código Haskell necesario para poder aplicar una función 
que dada la información de una temporada presente un listado
 (ordenado alfabéticamente según el nombre del torneo) con el nombre de los torneos,
  el jugador ganador y el número de sets jugados en la final.
-}												  	  
												  	  
--Guarda los nombres del torneo y de los jugadores.
type Anyo = String
type Nombre = String
type Jugador = String
type Sets = [Int]
type Torneo = (Nombre, Jugador, Jugador, Sets, Sets)
type Torneos = [Torneo]
type Temporadas = ([Anyo], Torneos)
type NumSets = Int
-- Tabla con los resultados que mostraremos por pantalla.
data Table =  Result Nombre Jugador NumSets deriving (Eq, Ord)

instance Show Table where
	show (Result n j numSets) = " " ++ show n ++ ", Ganador: " ++ show j ++ ", en " ++show numSets ++ "sets."


t :: Torneo
t = ("Open de Australia", "Novak Djokovic", "Andy Murray", [6, 7, 6, 6], [7, 6, 3, 2]) 

t2 :: Torneo
t2 = ("Indian Wells", "Juan Martin del Potro", "Rafael Nadal", [6, 3, 4], [4, 6, 6])

t3 :: Torneo
t3 = ("Mutua Madrid Open", "Rafael Nadal", "Stanislas Wawrinka", [6, 6], [2, 4])

t4 :: Torneo
t4 = ("Wimbledon", "Novak Djokovic", "Andy Murray", [6, 7, 6], [4, 5, 4])

mostrarListadoOrdenadoTorneos :: Temporadas -> Anyo -> String
mostrarListadoOrdenadoTorneos (_,[]) _ = error "Introducir Nombre de la Temporada"
mostrarListadoOrdenadoTorneos temp a = mostrar (funcionAux [] (torneos temp a))

mostrar :: [Table] -> String
mostart [] = ""
mostrar (x:xs) = show x ++ "\n" ++ mostrar xs

-- Tengo los torneos ordenados, solo tengo que devolver la tabla
funcionAux :: [Table] -> Torneos -> [Table]
funcionAux sol [] = sol
funcionAux sol ((n,j1,j2,sets1,sets2):xs) =  if ((winner(ganador(sets1,sets2))) == 1) then 
																					(Result n j1 (numJuegos(ganador(sets1, sets2)))):sol
																				else 
																					(Result n j2 (numJuegos(ganador(sets1, sets2)))):sol

winner :: (Int, Int) -> Int
winner (x, _) = x

numJuegos :: (Int, Int) -> Int
numJuegos (_,x) = x
-- Si devuelve 1, gana el primero, y 2 el segundo.
ganador :: (Sets, Sets) -> (Int, Int)
ganador = ganadorAux 0 0

--Funcion auxiliar, suma 1 al contador si gana un set el primero y resta 1 si gana el segundo. 
-- Al final si el contador es menor a 0 es que el segundo a ganado mas veces. y devuelve 2. 
-- Al final si el contador es mayor a 0 es que el primero a ganado mas veces. y devuelve 1. 
ganadorAux :: Int -> Int -> (Sets, Sets) -> (Int, Int)
ganadorAux pos cont ([], []) = if (cont > 0) then (1, pos) else (2, pos)
ganadorAux pos cont (l1, l2) = if (l1!!pos > l2!!pos) then ganadorAux (pos+1) (cont+1) (l1, l2) else ganadorAux (pos+1) (cont-1) (l1, l2)
-- la funcion torneos, devulve los torneos del año indicado ordenados alfabeticamente por
-- la funcion quicksort.
torneos :: Temporadas -> Anyo -> Torneos
torneos _ "" = []
torneos (_,[]) _ = []
torneos ((a:as), t) b =  if (b==a) then qs(t) else torneos (as,t) b

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (p:xs) = qs [ x | x <- xs, x < p] ++ [p] ++ qs [ x | x <-xs , x >= p]


{-
10) Dada la siguiente informacion aportada..
Se pide definir las instancias que permitan insertar valores de tipo Student en 
un árbol mediante el método insert. Nótese que aunque la definición del tipo 
Student utiliza la notación registro, sus valores pueden referenciarse utilizando 
los patrones habituales:
Student n a qs
-}
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving Show
data Student = Student {fullname :: String, age :: Int, qualifications :: [Int]} deriving Show
						
insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Empty x = Leaf x
insertTree (Node x l r) y = case compare y x of
												GT -> Node x l (insertTree r y)
												_ -> Node x (insertTree l y) r
instance Eq Student where
		(Student nombre edad notas) == (Student nombre2 edad2 notas2) = (edad==edad2)
		(Student nombre edad notas) /= (Student nombre2 edad2 notas2) = (edad/=edad2)
							
instance Ord Student where
		(Student nombre edad notas) < (Student nombre2 edad2 notas2) = (edad<edad2)
		(Student nombre edad notas) <= (Student nombre2 edad2 notas2) = (edad<=edad2)
		(Student nombre edad notas) > (Student nombre2 edad2 notas2) = (edad>edad2)
		(Student nombre edad notas) >= (Student nombre2 edad2 notas2) = (edad>=edad2)
							
{-
12) En un restaurante se desea gestionar las mesas libres y ocupadas de forma que 
se puedan asignar mesas rápidamente según llegan los comensales. 
De cada mesa se necesita conocer el número de mesa y su capacidad. 
Se pide implementar un tipo de datos Ocupacion, donde se puedan saber las mesas 
libres y las mesas ocupadas. 
La representación por pantalla del tipo ocupación debe ser la siguiente:
Libres:
[Mesa 1 -> Capacidad:10]
Ocupadas:
[Mesa 2 -> Capacidad:20]
Se pide además implementar las siguientes funciones:
-}

data Mesa = M {numero :: Int, capacidad :: Int} deriving (Eq, Ord)
type MesasLibres = [Mesa]
type MesasOcupadas = [Mesa]
data Ocupacion = Status MesasLibres MesasOcupadas deriving (Eq, Ord)

-- insertarMesaLibre: esta función, dada una ocupación y una mesa, 
-- inserta la mesa en la lista de mesas libres de forma ordenada, 
-- con las mesas con menor capacidad primero.
insertarMesaLibre :: Ocupacion -> Mesa -> Ocupacion
insertarMesaLibre (Status ((M nl cl):ls) ocuList) (M num cap) = (Status (insertBy compare (M num cap) ((M nl cl):ls)) (delete (M num cap) ocuList))     
 
-- ocuparMesa: dada una ocupación y un número de comensales, esta función devuelve 
-- una nueva ocupación, donde a los comensales se les ha asignado la mesa libre más 
-- pequeña en la que caben todos, y esta mesa ha sido añadida a la lista de mesas 
-- ocupadas y eliminada de la lista de mesas libres.
ocuparMesa :: Ocupacion -> Int -> Ocupacion
ocuparMesa (Status ls os) numPer = (Status (delete (mesaMinimaCap ls numPer) ls) (insert (mesaMinimaCap ls numPer) os))

mesaMinimaCap :: [Mesa] -> Int -> Mesa
mesaMinimaCap = mesaMinimaCapAux (M 0 0)

mesaMinimaCapAux :: Mesa -> [Mesa] -> Int -> Mesa
mesaMinimaCapAux (M ns cs) _ 0 = (M ns cs)
mesaMinimaCapAux (M ns cs) [] _ = (M ns cs)
mesaMinimaCapAux (M ns cs) ((M nm cm):ms) numPers = if (numPers==cm) then (M nm cm) else mesaMinimaCapAux (M nm cm) ms numPers

instance Show Mesa where
	show (M n c) = "[Mesa " ++ show n ++ " -> " ++ "Capacidad: " ++ show c ++ "]"
