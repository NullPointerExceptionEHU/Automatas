module Kdnf where
import Data.List
import Data.Char
import LibreriaAutomatas

kDnf_inicio::IO ()
kDnf_inicio = do n <- pedirNumVariables
		 k <- pedirValorK
		 let h = generarH n k
		 kDnf n k h
	  
kDnf:: Int -> Int -> [[Char]] -> IO ()
kDnf n k h = do putStr ("Es equivalente g con h = ")
		putStr (mostrarHache (mostrarXes h))
		putStrLn ("?")
                resp <- getLine
	        -- TODO Comprobar las respuestas de equivalencia
                if (head resp) == 's' then putStrLn ("fin. h = " ++ (mostrarHache (mostrarXes h)))
                else do valoracion <- pedirValoracion n
	                kDnf n k (eliminarTruesEnH n valoracion h)

eliminarTruesEnH:: Int -> [Char] -> [[Char]] -> [[Char]]
eliminarTruesEnH n v h = [ (eliminarTrue n v x) | x <- h]

eliminarTrue:: Int -> [Char] -> [Char] -> [Char]
eliminarTrue n v bloqueH 
	| comprobacion == [ '0' | x <- [1..n]] = comprobacion
	| otherwise = bloqueH
		where comprobacion = (eliminarTrueAux v bloqueH)

eliminarTrueAux:: [Char] -> [Char] -> [Char]
eliminarTrueAux v h
	| null v = []
	| head v == '1' && head h == '1' = '0':(eliminarTrueAux (tail v) (tail h))
	| head v == '1' && head h == '2' = '1':(eliminarTrueAux (tail v) (tail h))
	| head v == '0' && head h == '2' = '0':(eliminarTrueAux (tail v) (tail h))
	| head v == '0' && head h == '1' = '1':(eliminarTrueAux (tail v) (tail h))
	| otherwise = '0':(eliminarTrueAux (tail v) (tail h))
	
	
mostrarTermino::[[Char]] -> [Char]
mostrarTermino l
	|null l 								= error "lista vacia"
	|(null (tail l)) && ((head l) == "")	= "False"
	|(null(tail l)) && ((head l) /= "")		= head l
	|otherwise								= "("++(((head l) ++ " ^ ") ++ (mostrarTermino (tail l)))++")"

mostrarHache::[[[Char]]] -> [Char]
mostrarHache l
	|null l									= error "lista vacia"
	|null (tail l)							= mostrarTermino (head l)
	|otherwise								= (mostrarTermino (head l))++" v "++(mostrarHache (tail l))