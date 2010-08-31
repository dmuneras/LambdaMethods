module EcuacionesNoLineales where 
import GramaticaAbstracta
import GramaticaConcreta
import Semantica


--Función para ver si hay cambio de signo en el intervalo 
signo :: Func -> Func -> Func -> Bool
signo f x y 
          |(reduccion (FMult (sust f ('x',x)) (sust f ('x',y)))) < (FConst 0) = True
          | otherwise = False                                                             

-- Función que me dice si es raiz
raiz :: Func -> Func -> Bool         
raiz f x 
     | reduccion (sust f ('x',x)) == FConst 0  = True
     | otherwise = False

{- Función que realiza la búsqueda incremental de un intervalo que contenga almenos una raiz, recibe una función
   el valor inicial un diferencial y el numero de iteraciones que se van a realizar.
-}
busqdIncremental :: Func -> Func -> Func -> Integer -> [(Func,Func)]
busqdIncremental f a d 0 = []
busqdIncremental f a d i 
    | (raiz f a /= False) = [((reduccion a),(reduccion a))] 
    | (signo f a (FSum a d) /= False) = [((reduccion a),(reduccion(FSum a d)))] 
    | (otherwise) = (busqdIncremental f (FSum a d) d (i-1))


biseccion f xi xs e  tol iterMax
          | raiz f xi == True = "Encontre Raiz: " ++ (show xi) 
          | raiz f xs == True = "Encontre raiz: " ++ (show xs)
          | otherwise = biseccion' f xi xs  e tol iterMax 


--biseccion' :: Func -> Func -> Func -> Func -> Func -> Integer -> IO()
biseccion' f x y e tol iter 
    | (ym /= (FConst 0) && e > tol && iter>0) = if (signo f x y) 
                                  then (biseccion' f x xm e tol (iter-1)) 
                                  else (biseccion' f xm y e tol (iter-1))
    | (raiz f xm == True) = "Encontre Raiz: " ++ (show xm)
    | (e <= tol) = (show xm) ++ " es raiz con un error de" ++ (show e)
    | otherwise = "se sobrepaso el numero de iteraciones permitido"
    where 
          c = xm
          xm = reduccion (FDiv (FSum x y) (FConst 2))
          ym = (reduccion (sust f ('x',xm)))
          e = (FRes(xm)(c))
 

