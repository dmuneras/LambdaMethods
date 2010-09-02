module EcuacionesNoLineales where 
import GramaticaAbstracta
import GramaticaConcreta
import Semantica


--Función que retorna el error absoluto 
errorAbs ::Double-> Double -> Double
errorAbs ac an = abs ( ac - an)  

--Función para ver si hay cambio de signo en el intervalo 
signo :: Func -> Func -> Func -> Bool
signo f x y 
          |(reduccion (FMult (reduccion(sust f ('x',x))) (reduccion (sust f ('x',y))))) < (FConst 0) = True
          | otherwise = False                                                             

-- Función que me dice si es raiz
raiz :: Func -> Func -> Bool         
raiz f x 
     | reduccion (sust f ('x',x)) == FConst 0  = True
     | otherwise = False

{- Función que realiza la búsqueda incremental de un intervalo que contenga almenos una raiz, recibe una función
   el valor inicial un diferencial y el numero de iteraciones que se van a realizar.
   Basado en la función realizada por Carolina Campillo y Santiago Rodrigues
-}
busqdIncremental :: Func -> Func -> Func -> Integer -> Resp
busqdIncremental f a d 0 = (RSim (FConst 0))
busqdIncremental f a d i 
    | (raiz f a == True) = (RInt((reduccion a),(reduccion a))) 
    | (signo f a (FSum a d) == True) = (RInt((reduccion a),(reduccion(FSum a d)))) 
    | (otherwise) = (busqdIncremental f (FSum a d) d (i-1))

{- Función que realiza bisección, esta basado en la función realizada por Carolina Campillo y Santiago Rodrigues 
-} 
biseccion :: Func -> Func -> Func -> Double -> Integer -> String
biseccion f xi xs tol iterMax
          | raiz f xi == True = "Encontre Raiz: " ++ (show xi) 
          | raiz f xs == True = "Encontre raiz: " ++ (show xs)
          | (signo f xi xs == False)  = "El intervalo es inadecuado"
          | otherwise = biseccion' f xi xs (tol+1) tol (iterMax-1) 

{- función que realiza la parte recursiva de la bisección -}
biseccion' :: Func -> Func -> Func -> Double -> Double -> Integer -> String
biseccion' f x y  e tol iter 
    | (ym /= (FConst 0) && e > tol && iter>0) = if (signo f x xm) 
                                  then (biseccion' f x xm  e tol (iter-1)) 
                                  else (biseccion' f xm y  e tol (iter-1))
    | (raiz f xm == True) = "Encontre Raiz: " ++ (show xm)
    | (e <= tol) = (show xm) ++ " es raiz con un error de" ++ (show e)
    | otherwise = "se sobrepaso el numero de iteraciones permitido"
    where 
          xm' = xm
          xm = (reduccion (FDiv (FSum (x) (y)) (FConst 2)))
          ym = (reduccion (sust f ('x',xm)))
          e =  errorAbs (sacarNum(xm)) (sacarNum(xm'))


