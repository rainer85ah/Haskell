module FuncionesPatrones where
import Data.Char

-- Definir funciones por patrones..
-- Se ponen varios tipos de posibles argumentos y ella sabe cual ejecutarse.
-- Son funciones parciales.


-- Devuelve cierto si una lista tiene menor o igual a 3 elementos.

longitudMenor3 :: String -> Bool
longitudMenor3 (c1:c2:c3:c4:cs) = False
longitudMenor3 _ = True

-- Devuelve cierto si una cadena empieza por mayuscula
mayuscula :: String -> Bool
mayuscula [] = False
mayuscula (x:_) = isUpper x

-- Devuelve cierto si la cadena empieza por A o a.
empiezaA :: String -> Bool
empiezaA [] = False
empiezaA (x:_) = (x == 'a' || x == 'A')

-- Recibe y Devuelve dos cadenas ordenadas alfabeticamente.
cadenaOrdenada :: String -> String -> (String, String)
cadenaOrdenada lista [] = ([],lista) 
cadenaOrdenada [] lista = ([],lista) 
cadenaOrdenada (x:xs) (y:ys) = if x < y then (x:xs,y:ys) else (y:ys,x:xs)


