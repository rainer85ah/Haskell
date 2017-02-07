module ARBOLES where
import Data.List

-- Declaracion de Arbol de tipo poliformico.
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving (Show)

a1 :: Arbol Int
a1 = Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV))) 123 (Rama AV 10 AV)

-- Crear una funcion que devuelva una lista con las hojas.
hojas :: Arbol a -> [a]
hojas AV = [] 
hojas (Rama AV h AV) = [h]
hojas (Rama izq r der) = hojas izq ++ hojas der

-- Crear una funcion que diga si un arbol es hoja.
esHoja :: Arbol a -> Bool
esHoja (Rama AV _ AV) = True
esHoja _ = False

-- Crear una funcion que calcule el numero de hojas de un arbol.
numHojas :: Arbol a -> Int
numHojas arbol = length(hojas arbol) 
-- Vía Recursivo:
--numHojas AV = 0
--numHojas (Rama AV _ AV) = 1
--numHojas (Rama izq r der) = numHojas izq + numHojas der

-- Crear una funcion que devuelva el numero de nodos de un arbol.
numNodos :: Arbol a -> Int
numNodos AV = 0
numNodos (Rama izq _ der) = 1 + numNodos izq + numNodos der

-- Crear una funcion que devuleve la profundidad de un arbol, si es vacio nada.
profundidad :: Arbol a -> Int
profundidad (Rama AV a AV) = 1
profundidad (Rama izq _ der) = 1 + max (profundidad izq) (profundidad der)

-- Crear una funcion que diga si es un arbol es vacio o no.
-- Devuelve cierto si es una AVs..
esAV :: Arbol a -> Bool 
esAV AV = True
esAV _ = False

-- Redefinir la clase Eq dependiendo el tipo de dato que almacene
-- Declaracion de una clase y su igualdad de elementos del tipo a
instance Eq a => Eq (Arbol a) where
	AV == AV								    = True
	(Rama izq1 e1 der1) == (Rama izq2 e2 der2)	= (izq1 == izq2) && (e1 == e2) && (der1 == der2)
	_ == _ 									    = False

	AV /= (Rama _ _ _) 						    = True
	(Rama _ _ _) /= AV						    = True
	(Rama izq1 e1 der1) /= (Rama izq2 e2 der2)  = (izq1 /= izq2) || (e1 /= e2) || (der1 /= der2)
	_ /= _ 									    = False 

-- La Clase derivada (Ordering(compare), ya lo hace.. y devuelve LT,EQ,GT)
instance Ord a => Ord (Arbol a) where 
	Rama izq1 _ der1 <  Rama izq2 _ der2 = izq1<izq2 && der1<der2
	Rama izq1 _ der1 <= Rama izq2 _ der2 = izq1<=izq2 || der1<=der2
	Rama izq1 _ der1 >  Rama izq2 _ der2 = izq1>izq2 && der1>der2
	Rama izq1 _ der1 >= Rama izq2 _ der2 = izq1>=izq2 || der1>=der2

-- Crear una funcion que dado un Arbol x, lo devuelva en una lista.
treeInArray :: Arbol a -> [a]
treeInArray AV = []
treeInArray (Rama izq r der) = treeInArray izq ++ [r] ++ treeInArray der

-- Buscar un elemento en un arbol polimorfico..
buscar :: Eq a => Ord a => Arbol a -> a -> Bool
buscar AV _ = False
buscar (Rama izq r der) n 
					| (r == n) = True
					| (n < r) = buscar izq n
					| (n > r) = buscar der n
					| otherwise = False


--Funcion que guarda un arbol en una lista en sentido.. Recorrido PreOrder..
preOrder :: Arbol a -> [a]
preOrder AV = []
preOrder (Rama AV r AV) = [r]
preOrder (Rama izq r der) = [r] ++ preOrder izq ++ preOrder der

--Funcion que guarda un arbol en una lista en sentido.. Recorrido InOrder..
inOrder :: Arbol a -> [a]
inOrder AV = []
inOrder (Rama AV r AV) = [r]
inOrder (Rama izq r der) = inOrder izq ++ [r] ++ inOrder der

--Funcion que guarda un arbol en una lista en sentido.. Recorrido PostOrder..
postOrder :: Arbol a -> [a]
postOrder AV = []
postOrder (Rama AV a AV) = [a]
postOrder (Rama izq a der) = postOrder izq ++ postOrder der ++ [a]

-- Comprobar si un arbol esta ordenado ?????
arbolOrdenado :: (Eq a) => (Ord a) => Arbol a -> a -> Bool
arbolOrdenado AV _ = True
arbolOrdenado (Rama AV r AV) b = (r == b)
arbolOrdenado (Rama izq r der) b = if (r==b) then True else
															if (r < b) 
																	then arbolOrdenado izq b 
																	else arbolOrdenado der b
											
											
											
							 				

espejo :: Arbol a -> Arbol a
espejo AV = AV
espejo (Rama izq e der) = (Rama (espejo der) e (espejo izq))
											