module Integracion where

import GramaticaAbstracta
import Semantica
import EcuacionesNoLineales

--Funcion que halla la integral definida de una funcion por medio del metodo del Trapecio Sencillo
trapecioSen :: Func -> Func -> Func -> Func
trapecioSen f a b = reduccion (FMult (FDiv h (FConst 2)) (FSum fx0 fxn))
                    where h   = FRes b a 
                          fx0 = eval f ('x',a)
                          fxn = eval f ('x',b)

--Funcion que halla la integral definida de una funcion por medio del metodo del Trapecio Generalizado
trapecioGen :: Func -> Func -> Func -> Double -> Func
trapecioGen f a b n = let sum = foldl g (FConst 0) [1..(n-1)]
                      in reduccion (FMult (FDiv h (FConst 2)) (foldl1 (FSum) [fx0,(FMult (FConst 2) sum),fxn]))
                          where g sum i = FSum sum (eval f ('x',(x i)))
                                x i = FSum a (FMult (FConst i) h)
                                h   = FDiv (FRes b a) (FConst n)
                                fx0 = eval f ('x',a)
                                fxn = eval f ('x',b)

--Funcion que calcula la integral definida de una funcion por medio del metodo de Simpson Iterativo dado un numero de particiones minimo y una tolerancia. Esta funcion valida los datos de entrada y ordena llamar al ciclo principal
trapecioIter :: Func -> Func -> Func -> Double -> Func -> Integer -> String -> String
trapecioIter  f a b n tol iterMax typErr = if (even (truncate n)) then (trapecioIter' f a b n i0 e tol (iterMax-1) typErr)
                                           else (error "Numero de particiones impar")
                                               where i0 = trapecioSen f a b
                                                     e  = FSum tol (FConst 1)

--Funcion que ejecuta el metodo de Simpson Iterativo
trapecioIter' :: Func -> Func -> Func -> Double -> Func -> Func -> Func -> Integer -> String -> String
trapecioIter' f a b n i0 e tol iterMax typErr
              | (e > tol && iterMax > 0) = trapecioIter' f a b (n*2) i1 err tol (iterMax-1) typErr
              | (e <= tol) = "La integral de "++(show f)++" en el intervalo ["++(show a)++","++(show b)++"] es "++(show i0)++" con un error igual a "++(show e)
              | (otherwise) = "Se sobrepasaron las iteraciones"
              where i1 = trapecioGen f a b n
                    err = error' typErr i1 i0

--Funcion que halla la integral definida de una funcion por medio del metodo de Simpson 1/3 Sencillo
simpson13Sen :: Func -> Func -> Func -> Func
simpson13Sen f a b = reduccion (FMult (FDiv h (FConst 3)) (foldl1 (FSum) [fx0,(FMult (FConst 4) fxm),fxn]))
                     where h   = FDiv (FRes b a) (FConst 2)
                           m   = FDiv (FSum a b) (FConst 2)
                           fx0 = eval f ('x',a)
                           fxm = eval f ('x',m)
                           fxn = eval f ('x',b)

--Funcion que halla la integral definida de una funcion por medio del metodo de Simpson 1/3 Generalizado
simpson13Gen :: Func -> Func -> Func -> Double -> Func
simpson13Gen f a b n = if (even (truncate n)) then (let (sumi,sump) = foldl g (FConst 0,FConst 0) [1..(n-1)]
                 in  reduccion (FMult (FDiv h (FConst 3))(foldl1 (FSum) [fx0,(FMult(FConst 4) sumi),(FMult(FConst 2) sump),fxn]))) 
                       else (error "Numero de particiones impar")
                            where g (sumi,sump) i
                                      | even (truncate i)   = (sumi, FSum sump (eval f ('x',(x i))))
                                      | otherwise           = (FSum sumi (eval f ('x', (x i))), sump)
                                  x i                       = FSum a (FMult (FConst i)  h)
                                  h                         = FDiv (FRes b a) (FConst n)
                                  fx0                       = eval f ('x',a)
                                  fxn                       = eval f ('x', b)

--Funcion que halla la integral definida de una funcion por medio del metodo de Simpson 3/8 Sencillo
simpson38Sen :: Func -> Func -> Func -> Func
simpson38Sen f a b = reduccion (FMult (FDiv (FMult (FConst 3) h) (FConst 8)) (foldl1 (FSum) [fx0,(FMult(FConst 3)fx1),(FMult(FConst 3) fx2),fxn]))
                     where h = FDiv (FRes b a) (FConst 3)
                           x1  = FSum a h
                           x2  = FRes b h
                           fx0 = eval f ('x',a)
                           fx1 = eval f ('x',x1)
                           fx2 = eval f ('x',x2)
                           fxn = eval f ('x',b)

--Funcion que calcula la integral definida de una funcion por medio del metodo de Simpson Iterativo dado un numero de particiones minimo y una tolerancia. Esta funcion valida los datos de entrada y ordena llamar al ciclo principal
simpson13Iter :: Func -> Func -> Func -> Double -> Func -> Integer -> String -> String
simpson13Iter  f a b n tol iterMax typErr = if (even (truncate n)) then (simpson13Iter' f a b n i0 e tol (iterMax-1) typErr)
                                            else (error "Numero de particiones impar")
                                            where i0 = simpson13Sen f a b
                                                  e  = FSum tol (FConst 1)

--Funcion que ejecuta el metodo de Simpson Iterativo
simpson13Iter' :: Func -> Func -> Func -> Double -> Func -> Func -> Func -> Integer -> String -> String
simpson13Iter' f a b n i0 e tol iterMax typErr
               | (e > tol && iterMax > 0) = simpson13Iter' f a b (n*2) i1 err tol (iterMax-1) typErr
               | (e <= tol) = "La integral de "++(show f)++" en el intervalo ["++(show a)++","++(show b)++"] es "++(show i0)++" con un error igual a "++(show e)
               | (otherwise) = "Se sobrepasaron las iteraciones"
               where i1 = simpson13Gen f a b n
                     err = error' typErr i1 i0

--Funciones para pruebas
f1 :: Func --Para esta funcion probar con n = 12 que es el ejercicio hecho en clase
f1 = FDiv (FMult (FExp (FMult (FConst (-1)) (FVar 'x'))) (FLn (FRes (FPot (FVar 'x') (FConst 2)) (FConst 3)))) (FVar 'x')
a1 :: Func
a1 = FConst 4
b1 :: Func 
b1 = FConst 6.4
f2 :: Func --Para esta funcion probar con n = 6 que es el ejercicio del parcial
f2 = FMult (FExp (FSum (FMult (FConst (-1)) (FVar 'x')) (FConst 3))) (FLn (FVar 'x'))
a2 :: Func
a2 = FConst 5
b2 :: Func
b2 = FConst 7.4
--Para el iterativo
toli = FConst (1e-5)