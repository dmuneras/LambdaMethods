module EcuacionesNoLineales where

import GramaticaAbstracta
import GramaticaConcreta
import Semantica

{-FUNCIONES AUXILIARES
Estas funciones son utilizadas por los difetentes metodos lo modulo, son adaptadas de la practica del semestre 2010-1 realizada por Santiago Rodriguez y Carolina Campillo
-}--Funcion que determina si hay cambio de signo en la funcion evaluada en dos puntos
signo :: Func -> Func -> Func -> Bool
signo f a b
          |(reduccion (FMult (sust f ('x',a)) (sust f ('x',b)))) < (FConst 0) = True
          | otherwise = False                                                             

-- Funcion que determina si se esta parado en una raiz
raiz :: Func -> Func -> Bool     
raiz f x
     | reduccion (sust f ('x',x)) == FConst 0  = True
     | otherwise = False

{-METODO DE BUSQUEDAS INCREMENTALES
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
-- Funcion que realiza la busqueda incremental de un intervalo que contenga almenos una raiz
busqdIncremental :: Func -> Func -> Func -> Integer -> [(Func,Func)]
busqdIncremental f a d 0 = []
busqdIncremental f a d i
    | (raiz f a /= False) = [((reduccion a),(reduccion a))]
    | (signo f a (FSum a d) /= False) = [((reduccion a),(reduccion (FSum a d)))]
    | (otherwise) = (busqdIncremental f (FSum a d) d (i-1))

{-METODO DE BISECCION
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
-- Funcion que realiza los chequeos de entrada y que ordena iniciar el ciclo principal
biseccion :: Func -> Func -> Func -> Func -> Integer -> String
biseccion f xi xs tol n
    | (raiz f xi) = (show xi) ++ " es raiz" 
    | (raiz f xs) = (show xs) ++ " es raiz"
    | (not (signo f xi xs)) = "Intervalo incorrecto" 
    | otherwise = (biseccion' f xi xs (FConst 0) (reduccion (FSum (tol) (FConst 1))) tol (n-1))


--Funcion que realiza la biseccion 
biseccion' :: Func -> Func -> Func -> Func -> Func -> Func -> Integer -> String
biseccion' f xi xs xm'  e tol i
    | (ym /= (FConst 0) && e > tol && i>0) = if (not (signo f xi xm)) 
                                             then (biseccion' f xm xs xm err tol (i-1)) 
                                             else (biseccion' f xi xm xm err tol (i-1))
    | (raiz f xm) = (show xm) ++ " es raiz"
    | (e <= tol) = (show xm) ++ " es raiz con un error " ++ (show e)
    | otherwise = "El metodo no converge en las iteraciones dadas"
    where
          xm = (reduccion (FDiv (FSum xi xs) (FConst 2)))
          ym = (sust f ('x', xm))
          err = abs' (FRes xm xm')

{-METODO DE REGLA FALSA
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
-- Funcion que realiza los chequeos de entrada y que ordena iniciar el ciclo principal
reglaFalsa :: Func -> Func -> Func -> Func -> Integer -> String
reglaFalsa f a b tol n
    | (raiz f a) = (show a) ++ " es raiz" 
    | (raiz f b) = (show b) ++ " es raiz"
    | (not (signo f a b)) = "Intervalo incorrecto" 
    | otherwise = (biseccion' f a b (FConst 0)(reduccion (FSum (tol) (FConst 1))) tol (n-1))


--Funcion que realiza el metodo de regla falsa 
reglaFalsa' :: Func -> Func -> Func -> Func -> Func -> Func -> Integer -> String
reglaFalsa' f a b p' e tol i
    | (yp /= (FConst 0) && e > tol && i>0) = if (not (signo f a p)) 
                                             then (biseccion' f p b p' err tol (i-1)) 
                                             else (biseccion' f a p p' err tol (i-1))
    | (raiz f p) = (show p) ++ " es raiz"
    | (e <= tol) = (show p) ++ " es raiz con un error " ++ (show e)
    | otherwise = "El metodo no converge en las iteraciones dadas"
    where 
          p = (reduccion (FRes a (FDiv (FMult (reduccion (sust f ('x', a))) (FRes b a)) (FRes (reduccion (sust f ('x',b))) (reduccion (sust f ('x',a)))))))
          yp = (sust f ('x', p))
          err = abs' (FRes p p')

{-Se pueden probar las funciones de biseccion y regla falsa con esos parametros-}
fun :: Func
fun = (FRes (FPot (FVar 'x') (FConst 2)) (FConst 3))

a :: Func
a = (FConst (-2))

b :: Func
b = (FConst (-1.5))

tol :: Func
tol = (FConst (0.005))