module Dnfmonotonas where
import Data.List
import LibreriaAutomatas

pedirDatos:: IO()
pedirDatos = do putStrLn "Introduce un numero de variables: "
		putStr ">>"
		numVar <- getLine
		let nVar = (read numVar::Int)
		if (comprobarNumVar nVar) then dnfmonotonas ["0"] --h = false
		else pedirDatos

dnfmonotonas:: [[Char]] -> IO()
dnfmonotonas h = do 
		  putStrLn ("Es equivalente " ++ show (mostrarXes h) ++ " a g?")
		  resp <- getLine
		  if (head resp) == 's' then putStrLn ("fin. h = " ++ show (mostrarXes h))
		  else do putStrLn "Dame una valoracion: "
			  valoracion <- getLine
			  v <- dnfmonotonasaux valoracion 0 --Hecho todo lo posible con esa valoracion la guardamos en v para unirla a h
			  let h2 = h++[v]
			  dnfmonotonas h2 

dnfmonotonasaux:: [Char] -> Int -> IO [Char]
dnfmonotonasaux v cont = if (length [ x | x <- v, x == '1']) <= cont then return v
			 else do
				v1 <- return (cambiartrue v cont)
				resp <- (preguntarsivaloracion v1)
				if (head resp) == 'n' then (dnfmonotonasaux v (cont+1))
				else (dnfmonotonasaux v1 (cont+1))
				
cambiartrue:: [Char] -> Int -> [Char]
cambiartrue  v cont
	| null v = []
	| cont < 0 = v
	| (head v) == '1' && cont == 0 = '0':(cambiartrue (tail v) (cont-1))
	| (head v) == '1' = (head v):(cambiartrue (tail v) (cont-1))
	| otherwise = (head v):(cambiartrue (tail v) (cont))

preguntarsivaloracion:: [Char] -> IO [Char]
preguntarsivaloracion v = do putStrLn ("Esta v = " ++ v ++ " hace que g sea cierta?")
			     resp <- getLine
			     return resp

mostrarXes:: [[Char]] -> [[[Char]]]
mostrarXes l = [ mostrarXesaux x | x <- l]


mostrarXesaux:: [Char] -> [[Char]]
mostrarXesaux l = [ (mostrar x) | (x,y) <- zip ['1'..] l, y == '1']

mostrar:: Char -> [Char]
mostrar x = "X"++[x]

--Jose
mostrarTermino::[[Char]] -> [Char]
mostrarTermino l
	|null l 								= error "lista vacia"
	|(null (tail l)) && ((head l) == "")	= "False"
	|(null(tail l)) && ((head l) /= "")		= head l
	|otherwise								= "("++(((head l) ++ " ^ ") ++ (mostrarTermino (tail l)))++")"

putTermino::[Char] -> IO()
putTermino l = putStrLn l

mostrarHache::[[[Char]]] -> [Char]
mostrarHache l
	|null l									= error "lista vacia"
	|null (tail l)							= mostrarTermino (head l)
	|otherwise								= (mostrarTermino (head l))++" v "++(mostrarHache (tail l))
--Fin Jose
