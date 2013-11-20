module Kdnf where
import Data.List
import LibreriaAutomatas

-------------------Comments--------------------
-- 0 -> False
-- 1 -> True
-- 2 -> Negada
-----------------------------------------------

pedirNumVariables:: IO()
pedirNumVariables = do putStrLn "Introduce un numero de variables: "
		       putStr ">> "
		       numVar <- getLine
		       if not (isNum numVar) then pedirNumVariables
		       else putStr "" --Encontrar forma de no estar obligados a utilizar else (Reglas de Haskell)
		       
pedirValorK:: IO()
pedirValorK = do putStrLn "Introduce el valor de k: "
		 putStr ">> "
		 numVar <- getLine
		 if not (isNum numVar) then pedirValorK
		 else putStr "" --Encontrar forma de no estar obligados a utilizar else (Reglas de Haskell)