module EjerciciosHoja3 where
import Data.List;
import Data.Char;
import Data.String;
-- TYPE Y DATA..
-- Ejercicios sobre definici�n de tipos sin�nimos y nuevos tipos, 
-- tipos recursivos y tipos recursivos polim�rficos.

-- a) Se pide una funci�n que dada una lista de racionales, donde cada racional 
-- se define como dos n�meros enteros (numerador y denominador), 
-- y un n�mero racional, devuelva otra lista con todos los racionales equivalentes al dado.

-- Version con type:
type Racional = (Float, Float)

equiva :: Racional -> Racional -> Bool
equiva (a,b) (c,d) = (a/b == c/d)

equivalentes :: [Racional] -> Racional -> [Racional] 
equivalentes lista num = equivaAux lista num []

equivaAux :: [Racional] -> Racional -> [Racional] ->  [Racional]
equivaAux [] _ sol = sol
equivaAux (x:xs) numRac sol = if (equiva x numRac) then sol++[x] else equivaAux xs numRac sol


-- Version con data:
data Rac = R (Float, Float) deriving Show

eq :: Rac -> Rac -> Bool
eq (R (a,b)) (R (c,d)) = (a/b == c/d)

equiv :: [Rac] -> Rac -> [Rac] 
equiv lista (R num) = equivAux lista (R num) []

equivAux :: [Rac] -> Rac -> [Rac] -> [Rac]
equivAux [] (R _) sol = sol
equivAux ((R n):ns) (R numRac) sol = if (eq (R n) (R numRac)) then (R n):sol else equivAux ns (R numRac) sol


-- b) Se pide varias funciones para hacer lo siguiente:
-- 1. Funci�n que dado un punto de coordenadas y una direcci�n 
-- (Norte, Sur, Este u Oeste) mueva el punto hacia la direcci�n indicada. 
data Coordenada = C (Float, Float) deriving Show
data Movimiento = Norte | Sur | Este | Oeste deriving Show

mover :: Movimiento -> Coordenada -> Coordenada 
mover Norte (C (x,y)) = C (x,y+1)
mover Sur   (C (x,y)) = C (x,y-1)
mover Este  (C (x,y)) = C (x+1,y)
mover Oeste (C (x,y)) = C (x-1,y)
				

-- 2. Funci�n que dados dos puntos de coordenadas indique cu�l est� m�s al sur.
masAlSur :: Coordenada -> Coordenada -> Coordenada
masAlSur (C (x1,y1)) (C (x2,y2)) = if (y1<=y2) then (C (x1,y1)) else (C (x2,y2))


-- 3. Funci�n que calcule la distancia entre dos puntos:
-- Input: distancia (3,5) (6,7)
-- Output: 3.6055512

distancia :: Coordenada -> Coordenada -> Float
distancia (C (x1,y1)) (C (x2,y2)) = sqrt((x2-x1)^2 + (y2-y1)^2) 


-- 4. Funci�n que dado un punto y una lista de direcciones, retorne el camino 
-- que forman todos los puntos despu�s de cada movimiento sucesivo desde el punto original:
-- Input: camino (3,2,5,5) [Sur,Este,Este,Norte,Oeste]
-- Output: [(3.2,4.5),(4.2,4.5),(5.2,4.5),(5.2,5.5),(4.2,5.5)]

camino :: Coordenada -> [Movimiento] -> [Coordenada]
camino = caminoAux []

caminoAux :: [Coordenada] -> Coordenada -> [Movimiento] -> [Coordenada]
caminoAux sol _ [] = sol
caminoAux sol (C (x,y)) (dir:xs) = caminoAux (sol++[mover dir (C (x,y))]) (mover dir (C (x,y))) xs


{-
c) La empresa RealTimeSolutions, Inc. est� trabajando en un controlador para 
una central dom�tica. El controlador recibe informaci�n de termostatos situados
en diferentes habitaciones de la vivienda y bas�ndose en esta informaci�n, 
activa o desactiva el aire acondicionado en cada una de las habitaciones. 
Los termostatos pueden enviar la informaci�n sobre la temperatura en grados 
Celsius o Fahrenheit. A su vez, los aparatos de aire acondicionado reciben dos 
tipos de �rdenes: apagar y encender (on y off). Se pide:

1. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades.
2. Definir una funci�n convert que dada una temperatura en grados Celsius la 
convierta a grados Fahrenheit y viceversa. (Conversi�n de C a F: f = c * 9/5 + 32; 
conversi�n de F a C: c = (f � 32) * 5/9.)
3. Definir un tipo de datos para representar las �rdenes a los aparatos de a/a.
4. Definir una funci�n action que dada una temperatura en cierta habitaci�n 
determine la acci�n a realizar sobre el aparato de a/a de dicha habitaci�n. 
El controlador debe encender el aparato si la temperatura excede de 28�C. 
Ejemplos de aplicaci�n:
> action(Celsius(25)) > action(Fahrenheit(83.5))
On Off
-}
data Grado = Celsius Float | Fahrenheit Float deriving Show
data Mode = On | Off deriving Show

converter :: Grado -> Grado
converter (Celsius c) = (Fahrenheit (c*9/5+32))
converter (Fahrenheit f) = (Celsius ((f-32)*5/9))

action :: Grado -> Mode
action (Celsius c)    = if c<28 then On else Off
action (Fahrenheit f) = if f<82 then On else Off


{- D) Definir un tipo moneda para representar euros y d�lares USA. 
Definir una funci�n que convierta entre ambas monedas sabiendo que el factor 
de conversi�n de euros a d�lares es 1.14.
-}

data Money = E Float | D Float deriving Show

moneyConverter :: Money -> Money
moneyConverter (E e) = (D (e*1.14))
moneyConverter (D d) = (E (d*0.86))


{-
e) Dada el siguiente tipo de datos recursivo que representa expresiones aritm�ticas:
data Expr = Valor Integer | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr deriving Show

e.1) Se pide una funci�n para calcular el valor de una expresi�n.
e.2) Se pide una funci�n para calcular el n�mero de constantes de una expresi�n.
-}
data Expr = Valor Integer | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr deriving Show

calcValor :: Expr -> Integer
calcValor (Valor v) = v
calcValor (e1 :+: e2) = (calcValor e1) + (calcValor e2)
calcValor (e1 :-: e2) = (calcValor e1) - (calcValor e2)
calcValor (e1 :*: e2) = (calcValor e1) * (calcValor e2)

calcConst :: Expr -> Int
calcConst = calcConstAux 0

calcConstAux :: Int -> Expr -> Int
calcConstAux counter (Valor v) = counter+1
calcConstAux counter (e1 :+: e2) = (calcConstAux counter e1) + (calcConstAux counter e2)
calcConstAux counter (e1 :-: e2) = (calcConstAux counter e1) + (calcConstAux counter e2)
calcConstAux counter (e1 :*: e2) = (calcConstAux counter e1) + (calcConstAux counter e2)



{-
f) Dado el siguiente tipo de datos que representa un �rbol binario:
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

Se pide definir una funci�n que calcule el espejo de un �rbol.
Ejemplos de aplicaci�n de la funci�n ser�an:
> espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV))

> espejo (Rama AV 5 (Rama AV 4 AV))
Rama (Rama AV 4 AV) 5 AV
-}

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
a1 = (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
a2 = (Rama AV 5 (Rama AV 4 AV))

espejo :: Arbol a -> Arbol a
espejo AV = AV
espejo (Rama izq e der) = (Rama (espejo der) e (espejo izq))

