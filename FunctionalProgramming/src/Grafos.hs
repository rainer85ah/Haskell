module GRAFOS where
import Data.List (sort) 

type Adyacentes a = [a]
type Nodo a = (a, Adyacentes a)
data Grafo a = GV | G[Nodo a] deriving (Show)


devolverAdyacentes :: Grafo a -> Nodo a -> [Adyacentes a]
devolverAdyacentes [] _ = []
devolverAdyacentes ((a,b):c) v
			| (a == v) = b:(devolverAdyacentes c v)
			| (b == v) = a:(devolverAdyacentes c v)
			| otherwise = devolverAdyacentes c v
	
-- Comprobar si dos grafos son iguales.
grafosIguales :: (Eq a) => Grafo a -> Grafo a -> Bool
grafosIguales a b = a == b

-- Como se utiliza la IGUALDAD de un NUEVO TIPO DE DATOS, TENEMOS QUE DEFINIR
-- Eq para dicho tipo de dato!!
instance Eq a => Eq (Grafo a) where 
	g1 == g2 = igualesG g1 g2
	g1 /= g2 = not (igualesG g1 g2)

instance Eq Nodo where 
	a == b = igualesN a b
	a /= b = not (igualesN a b)	
	
instance Eq Adyacentes where 
	a == b = igualesAdy a b
	a /= b = not (igualesAdy a b)
	
igualesG :: (Eq a) => Grafo a -> Grafo a -> Bool
igualesG GVacio GVacio = True
igualesG GVacio _ = False
igualesG _ GVacio = False
igualesG (G(x,[xs])) (G(y,[ys])) = sort (x,[xs]) == sort (y,[ys])

igualesN :: (Eq a) => Nodo a -> Nodo a -> Bool
igualesN (v1,[]) (v2,[]) = v1==v2 && True
igualesN (v1,xs) (v2,[]) = False
igualesN (v1,[]) (v2,xs) = False
igualesN (v1,xs) (v2,ys) = v1==v2 && igualesAdy xs ys
  
igualesAdy :: (Eq a) => Adyacentes a -> Adyacentes a -> Bool
igualesAdy [] [] = True
igualesAdy [] _ = False
igualesAdy _ [] = False
igualesAdy xs ys = sort xs == sort ys
-- import Data.List (sort) , con esta libreria, organiza la lista y despues
-- Comprueba la igual de ambas listas..

--C)
sonAdyacentes :: (Eq a) => a -> a -> Grafo a -> Bool
sonAdyacentes _ _ GVacio = False
sonAdyacentes n1 n2 (G(n:[])) = adyacentes n1 n2 n 
sonAdyacentes n1 n2 (G(n:ns)) = adyacentes n1 n2 n || sonAdyacentes n1 n2 (G(n:[])) || sonAdyacentes n1 n2 (G(n:ns))

adyacentes :: (Eq a) => a -> a -> Nodo a -> Bool
adyacentes n1 n2 (nodo,lista) = (n1==nodo && contiene n2 lista) || (n2==nodo && contiene n1 lista)

contiene :: (Eq a) => a -> [a] -> Bool
contiene e [] = False
contiene e (x:xs) = e==x || contiene e xs
--Falta Comparar lista Adyacentes..
	
nValladolid :: Nodo String
nValladolid = ("Valladolid", ["Madrid"])

nMadrid :: Nodo String
nMadrid = ("Madrid", ["Valladolid", "Albacete", "Barcelona"])

nAlbacete :: Nodo String
nAlbacete = ( "Albacete", ["Madrid", "Valencia"] )

nValencia :: Nodo String
nValencia = ("Valencia", ["Albacete", "Barcelona"])

nBarcelona :: Nodo String
nBarcelona = ("Barcelona", ["Madrid", "Valencia"] )

grafo1 :: Grafo String
grafo1 = G [nMadrid, nValladolid, nAlbacete, nBarcelona]

grafo2 :: Grafo String
grafo2 = G [nMadrid, nValladolid, nAlbacete]	

grafo3 :: Grafo Int
grafo3 = GVacio
