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

