module EcuacionesNoLineales where

import GramaticaAbstracta
import GramaticaConcreta
import Semantica
import UU.Parsing

{-FUNCIONES AUXILIARES
Estas funciones son utilizadas por los difetentes metodos lo modulo, son adaptadas de la practica del semestre 2010-1 realizada por Santiago Rodriguez y Carolina Campillo
-}--Funcion que determina si hay cambio de signo en la funcion evaluada en dos puntos
signo :: Func -> Func -> Func -> Bool
signo f a b
          | reduccion ((sust f ('x',a)) */ (sust f ('x',b))) < (ton 0) = True
          | otherwise = False                                                             

-- Funcion que determina si se esta parado en una raiz
raiz :: Func -> Func -> Bool     
raiz f x
     | (eval f ('x',x)) == (ton 0)  = True
     | otherwise = False

--Funcion que retorna el valor de la derivada de un numero evaluada en un punto
derivada :: Func -> Func -> Func
derivada f x = reduccion ((eval f ('x',( x +/ h))) -/ (eval f ('x', x)) // h)
                where h = ton 0.0001

--Funcion que retorna la segunda derivada de una funcion, evaluada en un punto.
sdaDerivada :: Func -> Func -> Func
sdaDerivada f  n = reduccion(((eval f ('x', n +/ h))) -/ (ton 2 */ (eval f ('x',n))) +/ (eval f ('x',n -/ h)) // (h^/ (ton 2)))
                 where h = ton 0.0001

--Funcion que recibe el tipo de error a calcular, los valores actual y anterior y retorna el error deseado
error' :: String -> Func -> Func -> Func
error' t act ant
       | t == "abs" = eAbs act ant
       | t == "rel" = eRel act ant
       | otherwise  = error "No existe ese tipo de error"

--Funcion que calcula el error absoluto
eAbs :: Func -> Func -> Func
eAbs act ant = abs' (act -/ ant)

--Funcion que calcula el error relativo
eRel :: Func -> Func -> Func
eRel act ant = abs' (reduccion (act -/ ant) // act)

{-METODO DE BUSQUEDAS INCREMENTALES
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que realiza la busqueda incremental de un intervalo que contenga almenos una raiz
busqdIncremental :: Func -> Func -> Func -> Integer -> [(Func,Func)]
busqdIncremental f a d 0 = []
busqdIncremental f a d i
    | (raiz f a) = [((reduccion a),(reduccion a))]
    | (signo f a (a +/ d)) = [((reduccion a),(reduccion (a +/ d)))]
    | (otherwise) = (busqdIncremental f (a +/ d) d (i-1))
 
{-METODO DE BISECCION
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que realiza los chequeos de entrada y ordena iniciar el ciclo principal
biseccion :: Func -> Func -> Func -> Func -> Integer -> String -> String
biseccion f xi xs tol n typErr
    | (raiz f xi) = (show xi) ++ " es raiz" 
    | (raiz f xs) = (show xs) ++ " es raiz"
    | (not (signo f xi xs)) = "Intervalo incorrecto" 
    | (otherwise) = (biseccion' f xi xs (ton 0) (reduccion ((tol) +/ (ton 1))) tol (n-1) typErr)


--Funcion que realiza la biseccion 
biseccion' :: Func -> Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
biseccion' f xi xs xm' e tol i typErr
    | ((not (raiz f xm)) && e > tol && i>0) = if (not (signo f xi xm)) 
                                             then (biseccion' f xm xs xm err tol (i-1) typErr) 
                                             else (biseccion' f xi xm xm err tol (i-1) typErr)
    | (raiz f xm) = (show xm) ++ " es raiz"
    | (e <= tol) = (show xm) ++ " es raiz con un error " ++ (show e)
    | (otherwise) = "El metodo no converge en las iteraciones dadas"
    where xm = (reduccion ((xi +/ xs) // (ton 2)))
          err = error' typErr xm xm'

{-METODO DE REGLA FALSA
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que realiza los chequeos de entrada y ordena iniciar el ciclo principal
reglaFalsa :: Func -> Func -> Func -> Func -> Integer -> String -> String
reglaFalsa f a b tol n typErr
    | (raiz f a) = (show a) ++ " es raiz" 
    | (raiz f b) = (show b) ++ " es raiz"
    | (not (signo f a b)) = "Intervalo incorrecto" 
    | (otherwise) = (reglaFalsa' f a b (ton 0) (reduccion ((tol) +/ (ton 1))) tol (n-1) typErr)


--Funcion que realiza el metodo de regla falsa 
reglaFalsa' :: Func -> Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
reglaFalsa' f a b p' e tol i typErr
    | ((not (raiz f p)) && e > tol && i>0) = if (not (signo f a p)) 
                                             then (biseccion' f p b p err tol (i-1) typErr) 
                                             else (biseccion' f a p p err tol (i-1) typErr)
    | (raiz f p) = (show p) ++ " es raiz"
    | (e <= tol) = (show p) ++ " es raiz con un error " ++ (show e)
    | otherwise = "El metodo no converge en las iteraciones dadas"
    where p = (reduccion (a -/ (((eval f ('x', a)) */ (b -/ a)) // ((eval f ('x',b)) -/ (eval f ('x',a))))))
          err = error' typErr p p'


{-METODO DE PUNTO FIJO
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que recibe los datos y ordena iniciar el ciclo principal
puntoFijo :: Func -> Func -> Func -> Func -> Integer -> String -> String
puntoFijo f g x0 tol n typErr = (puntoFijo' f g x0 (reduccion ((tol) +/ (ton 1))) tol n typErr)

--Funcion que realiza el metodo de punto fijo
puntoFijo' :: Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
puntoFijo' f g x0 e tol i typErr
    | ((not (raiz f x1)) && e > tol && i > 0) = (puntoFijo' f g x1 err tol (i-1) typErr)
    | (raiz f x1) = (show x1) ++ " es raiz"
    | (e <= tol)  = (show x1) ++ " es raiz con un error " ++ (show e)
    | (otherwise) = "El metodo no converge en las iteraciones dadas"
    where x1 = eval g ('x', x0)
          err = error' typErr x1 x0

{-METODO DE NEWTON
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que recibe los datos y ordena iniciar el ciclo principal
newton :: Func -> Func -> Func -> Func -> Integer -> String -> String
newton f f' x0 tol n typErr = (newton' f f' x0 (reduccion ((tol) +/ (ton 1))) tol n typErr)

--Funcion que realiza el metodo de newton
newton' :: Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
newton' f f' x0 e tol  i typErr
    | ((not (raiz f x1)) && e > tol && (not (raiz f' x1)) && i > 0) = (newton'  f f' x1 err tol (i-1) typErr)
    | (raiz f x1) = (show x1) ++ "es raiz"
    | (e <= tol)  = (show x1) ++ " es raiz con un error " ++ (show e)
    | (raiz f' x1)= "La derivada se hizo cero -> Division por cero"
    | (otherwise) = "El metodo no converge en las iteraciones dadas"
    where x1 = reduccion (x0 -/ ((sust f ('x', x0)) // (sust f' ('x', x0))))
          err = error' typErr x1 x0

{-METODO DE LA SECANTE
NOTA: Esta adaptado del metodo implementado por Santiago Rodriguez y Carolina Campillo en la practica del semestre 2010-1, se hicieron los cambios necesarios para usarlo con nuestra gramatica de funciones
-}
--Funcion que realiza los chequeos de entrada y ordena iniciar el ciclo principal
secante :: Func -> Func -> Func -> Func -> Integer -> String -> String
secante f x0 x1 tol n typErr
        | (raiz f x0) = (show x0) ++ "es raiz"
        | (otherwise) = (secante' f x0 x1 (reduccion ((tol) +/ (ton 1))) tol (n-1) typErr)

--Funcion que realiza el metodo de la secante
secante' :: Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
secante' f x0 x1 e tol i typErr
	| ((not (raiz f x1)) && e > tol && denom /= (ton 0) && i>0) = (secante' f x1 x2 err tol (i-1) typErr)
	| (raiz f x1) = (show x1) ++ "es raiz"
	| (e <= tol) = (show x1) ++ "es raiz con un error " ++ (show e)
	| (denom == (ton 0)) = "El denominador se hizo cero"
	| (otherwise) = "El metodo no converge en las iteraciones dadas"
	where 	x2 = reduccion (x1 -/ ((y1 */ (x1 -/ x0)) // denom))
                denom = reduccion (y1 -/ y0)
		y0 = eval f ('x', x0)
                y1 = eval f ('x', x1)
		err = error' typErr x2 x1 

{-METODO DE RAICES MULTIPLES
Este metodo esta completamente desarrollado por nosotros ya que en la practica del semestre 2010-1 no esta definido
-}
--Funcion que recibe los datos e inicia el ciclo principal de raicesMultiples.
raicesMult :: Func -> Func -> Func ->Integer -> String -> String
raicesMult f a tol n typErr = (raicesMult' f a (reduccion tol +/ ton 1) tol n typErr)

--Funcion de raices multiples utilizando metodos numericos para hallar la primera y segunda derivada de la funcion.
raicesMult' :: Func -> Func -> Func -> Func -> Integer -> String -> String
raicesMult' f x0 e tol i typErr
           | ((not(raiz f x1)) && e > tol && den /= (ton 0) && i > 0) = (raicesMult' f x1 err tol (i-1) typErr)
           | (raiz f x1) = (show x1) ++ " es raiz"
           | (e <= tol) = (show x1) ++ " es raiz con un error " ++ (show e)
           | (den == (ton 0) ) = "Denomidador igual a 0"
           | otherwise = "El metodo no converge en las iteraciones dadas"
           where y = eval f ('x',x0)
                 den = reduccion (((derivada f x0) ^/ ton 2) -/ (y */ (sdaDerivada f x0)))
                 x1 = reduccion (x0 -/ ((y */ (derivada f x0)) // den))
                 err = error' typErr x1 x0

{-METODO DE RAICES MULTIPLES
Este metodo esta completamente desarrollado por nosotros ya que en la practica del semestre 2010-1 no esta definido
-}
--Funcion que recibe los datos y ordena iniciar el ciclo principal
raicesMultiples :: Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
raicesMultiples f f' f'' x0 tol n typErr = (raicesMultiples' f f' f'' x0 (reduccion ((tol) `FSum` (FConst 1))) tol n typErr)

raicesMultiples' :: Func -> Func -> Func -> Func -> Func -> Func -> Integer -> String -> String
raicesMultiples' f f' f'' x0 e tol i typErr
                 | ((not (raiz f x0)) && e > tol && i > 0) = (raicesMultiples' f f' f'' x1 err tol (i-1) typErr)
                 | (raiz f x0) = (show x0) ++ "es raiz"
                 | (e <= tol)  = (show x0) ++ "es raiz con un error " ++ (show e)
                 | (otherwise) = "El metodo no converge en las iteraciones dadas"
                 where x1 = reduccion (x0 `FRes` ((y `FMult` y') `FDiv` ((y' `FPot` (FConst 2)) `FRes` (y `FMult` y''))))
                       y  = eval f ('x', x0)
                       y' = eval f' ('x', x0)
                       y'' = eval f'' ('x', x0)
                       err = error' typErr x1 x0

{-Se pueden probar los diferentes metodos con estos parametros-}
--(X^2)-3
f :: Func
f = ((tov 'x') `FPot` (ton 2)) -/ (ton 3)

g :: Func
g = FRes (tov 'x') (FDiv (FRes (FPot (tov 'x') (ton 2.0)) (ton 3.0)) (FMult (ton 2.0) (tov 'x')))

f' :: Func
f' = (ton 2) */ (tov 'x')

a :: Func
a = (ton (-2))

b :: Func
b = (ton (-1.5))

tol :: Func
tol = (ton (1e-5))

{-Se puede probar el metodo de raices multiples con estas funciones-}
--COS^2(x)-2xCOS(x)+x^2
fr :: Func
fr = (((FCos (tov 'x')) ^/ (ton 2))) -/ (((ton 2)*/ (tov 'x'))*/ (FCos (tov 'x'))) +/ ((tov 'x') ^/ (ton 2))

fr' :: Func
fr' = FSum (FRes (FPot (FCos (FVar 'x')) (FConst 2.0)) (FMult (FMult (FConst 2.0) (FVar 'x')) (FCos (FVar 'x')))) (FPot (FVar 'x') (FConst 2.0))

--EXP(-2x)-2xEXP(-x)+x^2
fn :: Func
fn = FSum (FRes (FExp (FMult (FConst (-2.0)) (FVar 'x'))) (FMult (FMult (FConst 2.0) (FVar 'x')) (FExp (FMult (FConst (-1.0)) (FVar 'x'))))) (FPot (FVar 'x') (FConst 2.0))

fcos = (cos_(tov 'x') -/ (ton 2))

ff :: Func
ff = ((tov 'x') ^/ (ton 2)) -/ (ton 2)