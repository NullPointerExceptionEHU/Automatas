module Automatas where

import Data.List
--import LibreriaAutomatas
--import Dnfmonotonas
--import

mostrarEnunciado:: IO()
mostrarEnunciado = do
		putStrLn "Elige una de las 4 opciones: "
		putStrLn "1. DNF monotonas."
		putStrLn "2. k-CNFs."
		putStrLn "3. k-DNFs."
		putStrLn "4. Terminar"
		x <- pedirValor
		if x == 1 then putStrLn "dnfmonotonas"
			else if x == 2 then	putStrLn"kcnf"
					else if x == 3 then	putStrLn "kdnf"
							else if x == 4 then putStrLn "Saliendo..." else putStrLn "Saliendo..."
										--terminar

pedirValor:: IO Int
pedirValor = do
		putStrLn "Introduzca un valor entre 1 y 4 acorde a las opciones."
		n <- getLine
		if comprobarValor n ['1'..'4'] then return (read n :: Int)
		else
			pedirValor
		
comprobarValor:: [Char] -> [Char] -> Bool
comprobarValor l r
	|null r			= False
	|null l			= False
	|length l > 2	= False
	|otherwise		= length ([x|x <- l, x `elem` r]) == 1

	
{-terminar:: Int -> IO()
terminar = do
		putStrLn "Estas seguro de que quieres salir?"
-}
