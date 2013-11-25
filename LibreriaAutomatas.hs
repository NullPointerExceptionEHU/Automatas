module LibreriaAutomatas where
import Data.List

isNum:: [Char] -> Bool
isNum nums = (length [ x | x <- nums, x `elem` ['0'..'9']]) == (length nums) && nums /= ""
	
pedirNumVariables:: IO Int
pedirNumVariables = do putStrLn "Introduce un numero de variables: "
		       putStr ">> "
		       numVar <- getLine
		       if not (isNum numVar) || numVar == "0" then pedirNumVariables
		       else return (read numVar :: Int)
		       
pedirValorK:: IO Int
pedirValorK = do putStrLn "Introduce el valor de k: "
		 putStr ">> "
		 valK <- getLine
		 if not (isNum valK) || valK == "0" then pedirValorK
		 else return (read valK :: Int)
		 
-------------------Comments--------------------
-- 0 -> False
-- 1 -> True
-- 2 -> Negada

-- 100 -> X1
-- .
-- .
-- .
-----------------------------------------------

anadirCombinacion :: [Char] -> [[Char]] -> Int -> Int -> [[Char]]
anadirCombinacion alf leng n k
	| alf == [] = error "Alfabeto vacio"
	|otherwise   = [ x:y | x <- alf, y <- leng, (x:y) /= [ '0' | z <- [1..n]], length [ z | z <- (x:y), z `elem` "12"] <= k]


todasCombinacionLongN :: [Char] -> [[Char]] -> Int -> Int -> [[Char]]

todasCombinacionLongN alf leng n k
   | alf == []   = error "Alfabeto vacio" 
   | length (head leng) == n = leng
   | otherwise   = todasCombinacionLongN alf leng2 n k
   where leng2 = anadirCombinacion alf leng n k
   
generarH:: Int -> Int -> [[Char]]
generarH n k = todasCombinacionLongN "012" [""] n k

-- ===============================================================
{--
comprobarNumVar:: Int -> Bool
comprobarNumVar x
	| x<1 = False
	| otherwise = True
	
comprobarValor:: [Char] -> [Char] -> Int -> Bool
comprobarValor l r cant 
	| null r = False
	| null l = False
	| cant>length l = False
	| length ([x|x<-l, x `elem` r]) == cant	= True
	| otherwise = False
--}	

--pedirSiNo::[Char] -> Bool

{-- TODO Generalizar
pedirValoracion:: IO [Bool]
pedirValoracion = do
					putStrLn "Introduce una valoracion: "
					valoracion <- getLine
					if comprobarValor valoracion ['0', '1'] then
						let valoracionBool = (transformarCharABool valoracion)
						return valoracionBool
					else
						putStrLn "Incorrecto"
						pedirValoracion
--}		       
		       		       
transformarCharABool:: [Char] -> [Bool]
transformarCharABool l= [ (caracterABool x) | x <- l]

caracterABool:: Char -> Bool
caracterABool x
	| x == '0' = False
	| x == '1' = True
	| otherwise = error "Caracter no valido"
	
boolAInt:: Bool -> Int
boolAInt b
	| b == True = 1
	| b == False = 0
