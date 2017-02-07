-- Eq, Num, Ord, Show, Enum(succ,predecesor) .. Son utiles en funciones poliformicas, 
-- ya que puede tener los nuevos tipos de datos declarados por el usuario** 
-- Ejemplo el tipo de dato Persona = {nombre :: String, Edad :: Int} .
module ClasesyContexto where
import Data.List;

{--
–Eq tipos que definen igualdad (==) y (/=)
–Ord tipos que definen un orden (<=), (<), (>=), (>), min, max..
–Show tipos que se pueden mostrar por pantalla.
–Num tipos numéricos (+), (-), (*), (negate), (abs).
–Enum tipos enumerados (succ), (pred),..

–Eq tipos que definen igualdad: Bool, Char, Int, Integer, Float, …
–Ord tipos que definen un orden: Bool, Char, Int, Integer, Float, …
–Num tipos numéricos: Int, Integer, Float y Double..
--}

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

a1 = Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV))) 123 (Rama AV 10 AV)
a2 = Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV))) 50 (Rama AV 10 AV)

esIgual :: (Eq a) => Arbol a -> Arbol a -> Bool
esIgual a1 a2 = (a1==a2) 

instance Eq a => Eq (Arbol a) where
	AV == AV								    = True
	(Rama izq1 e1 der1) == (Rama izq2 e2 der2)	= (izq1 == izq2) && (e1 == e2) && (der1 == der2)
	_ == _ 									    = False

	AV /= (Rama _ _ _) 						    = True
	(Rama _ _ _) /= AV						    = True
	(Rama izq1 e1 der1) /= (Rama izq2 e2 der2)  = (izq1 /= izq2) || (e1 /= e2) || (der1 /= der2)
	_ /= _ 									    = False 
	

esMayor :: (Ord a) => Arbol a -> Arbol a -> Bool
esMayor a1 a2 = (a1>a2)

instance Ord a => Ord (Arbol a) where
	(Rama izq1 e1 der1) >  (Rama izq2 e2 der2) = (e1>e2)
	(Rama izq1 e1 der1) >= (Rama izq2 e2 der2) = (e1>=e2)
	
	(Rama izq1 e1 der1) <  (Rama izq2 e2 der2) = (e1<e2)
	(Rama izq1 e1 der1) <= (Rama izq2 e2 der2) = (e1<=e2)


-- Realizar una funcion que dada una lista de elementos devuelva el menor de ellos.
menor :: Ord a => [a] -> Maybe a
menor [] = Nothing
menor (x:[]) = Just x
menor (x:y:xs) = menor ((min x y):xs)

-- Realizar una funcion que dado un elemento y una lista de tuplas de dos elemtos, busque la tupla donde el elemento
-- dado esta en primer lugar y devuleva su pareja en la tupla. 

pareja :: Eq a => a -> [(a,b)] -> Maybe b
pareja _ [] = Nothing
pareja b ((x,z):xs) = if (b==x) then Just z else pareja b xs

-- Realizar una funcion que dada una lista de elementos de cualquier tipo y un 
-- elementos de ese mismo tipo devuelva la posicion en la que se encuentra el elemento.

index2 :: Eq a => a -> [a] -> Maybe Int
index2 x = index2Aux x 0

index2Aux :: Eq a => a -> Int -> [a] -> Maybe Int
index2Aux _ _ [] = Nothing
index2Aux e pos (x:xs) = if (x==e) then Just pos else index2Aux e (pos+1) xs


