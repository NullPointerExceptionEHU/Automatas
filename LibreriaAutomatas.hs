module LibreriaAutomatas where
import Data.List

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
	

--pedirSiNo::[Char] -> Bool

	
pedirValoracion:: IO [Bool]
pedirValoracion = do
					putStrLn "Introduce una valoracion: "
					valoracion <- getLine
					if comprobarValor valoracion ['0', '1'] then
						valoracionBool = (transformarCharABool valoracion)
						return valoracionBool
					else
						putStrLn "Incorrecto"
						pedirValoracion
		       
		       		       
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
