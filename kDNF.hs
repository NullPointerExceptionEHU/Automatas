module Kdnf where
import Data.List
import LibreriaAutomatas

-------------------Comments--------------------
-- 0 -> False
-- 1 -> True
-- 2 -> Negada
-- 3 -> No negada
-----------------------------------------------
-- k*n^k = Total de terminos a examinar
generarH:: Int -> Int -> [Int] -- n = 2  y K = 2 => [[3],[2],[3],[2],[3,2],[2,2],[2,3],[3,3]]
generarH n k = 

-- Dados un alfabeto y un lenguaje, esta función genera el 
-- lenguaje que contiene todas las palabras que se obtienen  
-- añadiendo un símbolo por la izquierda a cada palabra del 
-- lenguaje de entrada

anadir :: Alfabeto -> Lenguaje -> Lenguaje
-- "abc" [ca, bbc]
anadir alf leng    
   |alf == []   = error "Alfabeto vacio" 
   |otherwise   = [ x:y | x <- alf, y <- leng]
   
----------------------------------------------------------

-- Dados un alfabeto y un lenguaje, esta función genera el 
-- lenguaje que contiene todas las palabras del lenguaje de  
-- entrada y todas las palabras que se obtienen añadiendo  
-- cualquier cantidad de símbolos por la izquierda a cada 
-- palabra del lenguaje de entrada

todas :: Alfabeto -> Lenguaje -> Lenguaje

todas alf leng 
   |alf == []   = error "Alfabeto vacio" 
   |otherwise   = leng++(todas alf leng2)
   where leng2 = anadir alf leng

----------------------------------------------------------

----------------------------------------------------------
--FUNCIÓN QUE GENERA EL LENGUAJE UNIVERSAL PARA UN ALFABETO
----------------------------------------------------------

----------------------------------------------------------
-- Dado un alfabeto generar el lenguaje universal correspondiente

univ :: Alfabeto -> Lenguaje


univ alf
  |alf == []  = error "Alfabeto vacio"
  |otherwise  = todas alf [""]
