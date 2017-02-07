{-
Listado de ejercicios para poner en práctica los conocimientos adquiridos sobre
definición de tipos sinónimos y nuevos tipos, tipos recursivos y tipos 
recursivos polimórficos. Y también sobre el manejo de clases de tipos en Haskell.
-}
module EjerciciosHojas4 where

{-
a) Definir una función que dado un día de la semana, indique si éste es o no 
laborable. Para representar el día de la semana se deberá crear un nuevo tipo enumerado.
-}
data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

laboral :: Dia -> Bool
laboral Sabado = False
laboral Domingo = False
laboral _ = True


{-
b) Se quiere ordenar los elementos de una lista (cuyos elementos son comparables) 
mediante el algoritmo del quicksort.
-}

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let menoresOrdenados = quicksort [a | a <- xs, a <= x]  
        mayoresOrdenados = quicksort [a | a <- xs, a > x]  
    in  menoresOrdenados ++ [x] ++ mayoresOrdenados 
    
    
   
{-
c) Se pide implementar una función que dada un número (de cualquier tipo que 
soporte la operación de división) y una lista de números del mismo tipo, 
divida a ese número por cada uno de los elementos contenidos en la lista y 
devuelva una lista con el resultado.
Ejemplos de aplicación de la función son:
> divisiones 5 [1,2,3]
[Just 5,Just 2,Just 1]
> divisiones 5 [1,2,3,0,9,10]
[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]
-}
divisiones :: (Eq a) => (Integral a) => a -> [a] -> [Maybe a]
divisiones n = divisionesAux [] n
 
divisionesAux :: (Eq a) => (Integral a) => [Maybe a] -> a -> [a] -> [Maybe a]
divisionesAux sol n [] = sol
divisionesAux sol 0 _ = sol
divisionesAux sol n (x:xs) = if (x==0) then 
											divisionesAux (sol++[Nothing]) n xs 
										else 
											divisionesAux (sol++[Just (div n x)]) n xs


{-
d) Dado un nuevo tipo de datos para representar un árbol binario de cualquier tipo, definido como sigue:
data Arbol a = AV | Rama (Arbol a) a (Arbol a)
Se pide definir una función que visualice el árbol por pantalla de una 
determinada forma: separando cada hijo izquierdo y derecho por “|”, 
la raíz entre guiones y cada nivel diferente del árbol por “( )”. 
Ejemplos de aplicación de la función sería los siguientes:
> mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
"((60)|-8-|())|-5-|(4)"
-}

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

mostrarArbol :: (Show a) => Arbol a -> String
mostrarArbol =  mostrarArbolAux ""

mostrarArbolAux :: (Show a) => String -> Arbol a -> String
mostrarArbolAux cad AV = cad
mostrarArbolAux cad (Rama AV r AV) = cad ++ show r
mostrarArbolAux cad (Rama izq r der) = cad++"("++ mostrarArbolAux cad izq ++")|-"++ show r ++ "-|("++ mostrarArbolAux cad der ++ ")"



{-
e) Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados 
en una universidad que pertenezcan a alguna de las asociaciones de ésta
(culturales, deportivas,de representación estudiantil, etc.). 
Para ello se deberán crear nuevos tipos de datos que representen:
Estudiante, de cada uno se debe disponer del nombre y titulación
Titulación, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE
Lista de estudiantes matriculados
Lista de estudiantes que pertenecen a asociaciones
Un ejemplo de aplicación de la función que se pide podría ser:
> mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)
"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
Donde Carlos Calle e Irene Plaza son los únicos estudiantes matriculados que 
pertenecen a algún tipo de asociación en la universidad.
-}


type Nombre = String
data Titulaciones = GradoII | GradoII_ADE | GradoADE deriving Show
data Asociaciones = Culturales | Deportivas | Estudiantil deriving (Show)
type Asociado = (Nombre, Asociaciones)
type Estudiante = (Nombre, Titulaciones)

listaMatriculados :: [Estudiante]
listaMatriculados = [("Rainer Arencibia", GradoADE),
					 ("Sofia Serrano", GradoII), 
					 ("Silvia Garcia", GradoADE),
					 ("Alex Martin", GradoII_ADE) 
					]

listaAsociaciones :: [Asociado]
listaAsociaciones = [("Rainer Arencibia", Deportivas), 
					 ("Sofia Serrano", Culturales), 
					 ("Silvia Garcia", Estudiantil), 
					 ("Alex Martin", Estudiantil)
					]

mostrarAlumnosAsociaciones :: ([Estudiante], [Asociado]) -> [Estudiante]
mostrarAlumnosAsociaciones = mostrarAux []

mostrarAux :: [Estudiante] -> ([Estudiante], [Asociado]) -> [Estudiante]
mostrarAux sol ([], _) = sol
mostrarAux sol (_, []) = sol
mostrarAux sol (((a1,a2):as), ((c1,c2):cs)) = if ((a1 == c1) && (c2==Deportivas || c2==Culturales)) 
												then 
													mostrarAux (sol++[(a1,a2)]) (as,cs)
								                else 
								                	mostrarAux sol (as,cs)


instance Eq Asociaciones where
		Culturales==Culturales	= True
		Deportivas==Deportivas	= True
		Estudiantil==Estudiantil= True
		_ == _ 					= False
		
		Culturales/=Culturales	= False
		Deportivas/=Deportivas	= False
		Estudiantil/=Estudiantil= False
		_ /= _ 					= True
		
		

{-
f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, 
para ello se deberá crear un nuevo tipo de datos en Haskell. 
Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos es Fecha, 
en el intérprete al poner fechas concretas nos devolvería la representación 
de la fecha que hayamos definido:
> Fecha 10 10 2013 > Fecha 24 12 2012
10/10/2013 			24/12/2012
-}
type Diia = Int
type Mes = Int
type Anyo = Int

data Date = Fecha Diia Mes Anyo

instance Show Date where
		 show (Fecha d m a) 
			 | (d > 0 && d <= 31) && (m > 0 && m <= 12) && (a >= 1900 && a <= 2200) = (show d ++ "/" ++ show m ++ "/" ++ show a)
			

{-
g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una función que sea capaz de comparar dos fechas. Ejemplos de aplicación de la función serían:
> mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
True
> mismaFecha (Fecha 10 11 2013) (Fecha 10 10 2013)
False
-}

mismaFecha :: (Date) -> (Date) -> Bool
mismaFecha (Fecha d1 m1 a1) (Fecha d2 m2 a2) = ((Fecha d1 m1 a1) == (Fecha d2 m2 a2))


{-
h) Teniendo en cuenta la definición de la función qs del apartado (b) de este listado de ejercicios, 
se pide ordenar una lista de fechas mediante quicksort. 
Ejemplos de aplicación de la función serían:
> qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 12 12 2013)]
[24/12/2012,10/9/2013,10/10/2013,12/12/2013]
-}
qs :: [Date] -> [Date]
qs = quicksort  
	
instance Ord Date where
	(Fecha d1 m1 a1) > (Fecha d2 m2 a2) = (a1>a2 || (a1>a2 && m1>m2) || (a1>a2 && m1>m2 && d1>d2))
	(Fecha d1 m1 a1) >= (Fecha d2 m2 a2) = (a1>=a2 || (a1>=a2 && m1>=m2) || (a1>=a2 && m1>=m2 && d1>=d2))
	(Fecha d1 m1 a1) < (Fecha d2 m2 a2) = (a1<a2 || (a1<a2 && m1<=m2) || (a1<a2 && m1<m2 && d1<d2))
	(Fecha d1 m1 a1) <= (Fecha d2 m2 a2) = (a1<=a2 || (a1<=a2 && m1<=m2) || (a1<=a2 && m1<=m2 && d1<=d2))
	

instance Eq Date where
	(Fecha d1 m1 a1) == (Fecha d2 m2 a2) = (a1==a2 && m1==m2 && d1==d2)
	(Fecha d1 m1 a1) /= (Fecha d2 m2 a2) = not (a1==a2 && m1==m2 && d1==d2)
	
	
	
{-
i) Se pide crear una nueva clase de tipos, llamada Coleccion, para representar 
colecciones de datos de cualquier tipo, donde los tipos pertenecientes a esta
 clase tendrán el siguiente comportamiento:
 esVacia: función para saber si la colección está vacía.
insertar: insertará un nuevo elemento en la colección.
primero: devolverá el primer elemento de la colección.
eliminar: eliminará un elemento de la colección.
size: devolverá el número de elementos de la colección.
Algunas de las funciones anteriores variarán su implementación en función del 
tipo de colección particular que sea instancia de la clase Coleccion. Por ello, 
se pide crear dos instancias diferentes de esta clase para los dos nuevos
 tipos de datos que se presentan a continuación:
data Pila a = Pil [a] deriving Show
data Cola a = Col [a] deriving Show
El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. 
El segundo representa una estructura de datos FIFO de elementos de tipo a.
-}
data Pila a = P [a] deriving (Show, Eq, Ord)
data Cola a = C [a] deriving (Show, Eq, Ord)

class Coleccion t where
	esVacia :: t a -> Bool
	insertar :: a -> t a -> t a
	primero :: t a -> a
	eliminar :: t a -> t a
	size :: t a -> Int
	
instance Coleccion Pila where 
	esVacia (P p) = ((length p) == 0)
	insertar e (P p) = (P (p++[e]))
	primero (P p) = last p
	eliminar (P p) = (P (init p))
	size (P p) = length p
	
instance Coleccion Cola where 
	esVacia (C c) = ((length c) == 0)
	insertar e (C c) = (C (c++[e]))
	primero (C c) = head c
	eliminar (C c) = (C (drop 1 c))
	size (C c) = length c
