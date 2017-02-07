
module List where
import Data.List;

data Lista a = L [a] deriving (Show, Eq, Ord)
data Pila a = P [a] deriving (Show, Eq, Ord)
data Cola a = C [a] deriving (Show, Eq, Ord)
data Matriz a = M [a] deriving (Show, Eq, Ord)

class Coleccion t where
	esVacia :: t a -> Bool
	insertar :: a -> t a -> t a
	primero :: t a -> a
	ultimo :: t a -> a
	eliminar :: t a -> t a
	size :: t a -> Int
	
instance Coleccion Lista where
	esVacia (L lista) = (length lista) == 0
	insertar e (L lista) = (L (lista++[e])) 
	primero (L lista) = head lista
	ultimo (L lista) = last lista
	eliminar (L lista) = (L (drop 1 lista))
	size (L lista) = length lista
	
instance Coleccion Pila where
	esVacia (P pila) = (length pila) == 0
	insertar e (P pila) = (P (pila++[e]))
	primero (P pila) = last pila
	ultimo (P pila) = head pila
	eliminar (P pila) = (P (init pila))
	size (P pila) = length pila

instance Coleccion Cola where
	esVacia (C cola) = (length cola) == 0
	insertar e (C cola) = (C (cola++[e]))
	primero (C cola) = head cola
	ultimo (C cola) = last cola
	eliminar (C cola) = (C (tail cola))
	size (C cola) = length cola
	
	
