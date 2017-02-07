-- Ejemplos de composicion de diferentes funciones
module FuncionesComposiciones where

suma :: Int -> Int -> Int
suma x y = x + y

doble :: Int -> Int
doble x = x*2

-- PARCIAL
composicion :: Int -> Int -> Int
-- Dos enteros como argumentos, primero la funcion doble
-- del primer argumento y al resultado le suma el 2do argumento.
composicion = suma.doble 
-- compisicion 2 3
-- doble 2 = 4
-- suma 3 4
-- 7

-- NO PARCIAL
--cuatruple :: Int -> Int
--cuatruple x = doble.(doble x) 

-- PARCIAL
cuatruple :: Int -> Int
cuatruple = doble.doble
 
 --Composicion de Funciones
composicionTres :: Int -> Int -> Int
composicionTres x = negate.(suma x).doble
-- compTres 2 3 = -8!! 