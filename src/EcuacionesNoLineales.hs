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

--Funcion que retorna el valor de la derivada de un numero evaluada en un punto
derivada :: Func -> Func -> Func
derivada f x = reduccion ((((sust f ('x',( x +/ h)))) -/ (reduccion (sust f ('x', x)))) // h)
                where h = ton 0.0001
--Funcion que retorna la segunda derivada de una funcion, evaluada en un punto.
sdaDerivada :: Func -> Func -> Func
sdaDerivada f  n = reduccion(((reduccion (sust f ('x', n +/ h))) -/ (ton 2 */ (reduccion (sust f ('x',n)))) +/ (reduccion (sust f ('x',n -/ h)))) // (h^/ (ton 2)))
                 where h = ton 0.000001

{-METODO DE BUSQUEDAS INCREMENTALES
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
-- Funcion que realiza la busqueda incremental de un intervalo que contenga almenos una raiz
busqdIncremental :: Func -> Func -> Func -> Integer -> [(Func,Func)]
busqdIncremental f a d 0 = []
busqdIncremental f a d i
    | (raiz f a /= False) = [((reduccion a),(reduccion a))]
    | (signo f a ( a +/ d) /= False) = [((reduccion a),(reduccion (a +/ d)))]
    | (otherwise) = (busqdIncremental f ( a +/ d) d (i-1))

{-METODO DE BISECCION
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
-- Funcion que realiza los chequeos de entrada y que ordena iniciar el ciclo principal
biseccion :: Func -> Func -> Func -> Func -> Integer -> String
biseccion f xi xs tol n
    | (raiz f xi) = (show xi) ++ " es raiz" 
    | (raiz f xs) = (show xs) ++ " es raiz"
    | (not (signo f xi xs)) = "Intervalo incorrecto" 
    | otherwise = (biseccion' f xi xs (ton 0) (reduccion (tol +/ ton 1)) tol (n-1))


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
          xm = (reduccion ((xi +/xs)//ton 2))
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
    | (err <= tol) = (show p) ++ " es raiz con un error " ++ (show e)
    | otherwise = "El metodo no converge en las iteraciones dadas"
    where 
          p = (reduccion (FRes a (FDiv (FMult (reduccion (sust f ('x', a))) (FRes b a)) (FRes (reduccion (sust f ('x',b))) (reduccion (sust f ('x',a)))))))
          yp = (sust f ('x', p))
          err = abs' ( p -/ p')

--Funcion que recibe los datos y ordena iniciar el ciclo principal
puntoFijo :: Func -> Func -> Func -> Func -> Integer -> String
puntoFijo f g x0 tol n = puntoFijo' f g x0 (reduccion ((tol) `FSum` (FConst 1))) tol n

--Funcion que realiza el metodo de punto fijo
puntoFijo' :: Func -> Func -> Func -> Func -> Func -> Integer -> String
puntoFijo' f g x0 e tol i
    | ((not (raiz f x1)) && e > tol && i > 0) = puntoFijo' f g x1 err tol (i-1)
    | (raiz f x1) = (show x1) ++ " es raiz"
    | (e <= tol)  = (show x1) ++ " es raiz con un error " ++ (show e)
    | (otherwise) = "El metodo no converge en las iteraciones dadas"
    where x1 = reduccion (sust g ('x', x0))
          err = abs' (x1 -/ x0)


--Funcion que recibe los datos e inicia el ciclo principal de raicesMultiples.
raicesMult :: Func -> Func -> Func ->Integer -> String
raicesMult f a tol i = raicesMult' f a (tol +/ ton 1) tol i

--Funcion de raices multiples utilizando metodos numericos para hallar la primera y segunda derivada de la funcion.
raicesMult' :: Func -> Func -> Func -> Func -> Integer -> String
raicesMult' f x0 e tol i 
           | ((not(raiz f x1)) && e > tol && den /= (ton 0) && i > 0) = raicesMult' f x1 err tol (i-1)
           | (raiz f x1) = (show x1) ++ " es raiz"
           | (e <= tol) = (show x1) ++ " es raiz con un error " ++ (show e)
           | (den == (ton 0) ) = "Denomidador igual a 0"
           | otherwise = "El metodo no converge en las iteraciones dadas"
           where y = reduccion(sust f ('x',x0))
                 den = reduccion (((derivada f x0) ^/ ton 2) -/ (y */ (sdaDerivada f x0)))
                 x1 = reduccion (x0 -/ ((y */ (derivada f x0)) // den))
                 err = abs'(x1 -/ x0)
           
 
{-FUNCIONES PARA PROBAR METODOS -}

{-Se pueden probar las funciones de biseccion y regla falsa con esos parametros-}

funb :: Func
funb = tov 'x' ^/ ton 2 -/ ton 3

a1 :: Func
a1 =  ton (-2)

b :: Func
b = ton (-1.5)

tol :: Func
tol = ton (0.005)

{- Funcion para evaluar el metodo de raices multiples -}
frm :: Func 
frm = (((FCos (tov 'x')) ^/ (ton 2))) -/ (((ton 2)*/ (tov 'x'))*/ (FCos (tov 'x'))) +/ ((tov 'x') ^/ (ton 2))
 



