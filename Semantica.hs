module Semantica where

import GramaticaConcreta
import GramaticaAbstracta

{-Funcion que sustituye una variable en una funcion
-}
sust :: Func -> (Char, Func) -> Func
sust a@(FConst c) _ = a
sust a@(FVar x) (var,f)
      | x == var      = f
      | otherwise     = a 
sust (FSum a b) s@(var,f)  = FSum (sust a s) (sust b s)
sust (FRes a b) s@(var,f)  = FRes (sust a s) (sust b s)
sust (FMult a b) s@(var,f) = FMult (sust a s) (sust b s)
sust (FDiv a b) s@(var,f)  = FDiv (sust a s) (sust b s)
sust (FPot a b) s@(var,f)  = FPot (sust a s) (sust b s)
sust (FExp a) s@(var, f)   = FExp (sust a s)
sust (FLn a)  s@(var, f)   = FLn  (sust a s)
sust (FSen a) s@(var, f)   = FSen (sust a s)
sust (FCos a) s@(var, f)   = FCos (sust a s)
sust (FTan a) s@(var, f)   = FTan (sust a s)
sust (FSec a) s@(var, f)   = FSec (sust a s)
sust (FCsc a) s@(var, f)   = FCsc (sust a s)
sust (FCot a) s@(var, f)   = FCot (sust a s)

	
{-Funcion que determina si una funcion es constante          
-}
isCons :: Func -> Bool
isCons (FConst a) = True
isCons _          = False

{-Funcion que saca el valor de una funcion constante
-}
sacarNum :: Func -> Double
sacarNum (FConst a) = a
sacarNum _          = error "No const"

{-Funcion que reduce los terminos de una funcion y de sus subterminos
-}          
eval :: Func -> Func
eval (FConst c) = FConst c
eval (FVar x)   = FVar x
eval (FSum a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)+ sacarNum (b))
		| otherwise                =  FSum (eval a) (eval b)
eval (FRes a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)- sacarNum (b))
		| otherwise                =  FRes (eval a) (eval b)
eval (FMult a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)* sacarNum (b))
		| otherwise                =  FMult (eval a) (eval b)
eval (FDiv a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)/ sacarNum (b))
		| otherwise                = FDiv (eval a) (eval b)
eval (FPot a b)
		| (isCons a) && (isCons b) = FConst ((sacarNum (a)**(sacarNum (b))))
		| otherwise                = FPot (eval a) (eval b)
eval (FExp a)
		| isCons a  = FConst ((2.7182)**(sacarNum (a)))
		| otherwise = FExp (eval a)
eval (FLn a)
		| isCons a  = FConst (log (sacarNum (a)))
		| otherwise = FLn (eval a)
eval (FSen a)
		| isCons a  = FConst (sin (sacarNum (a)))
		| otherwise = FSen (eval a)
eval (FCos a)
		| isCons a  = FConst (cos (sacarNum (a)))
		| otherwise = FCos (eval a)
eval (FTan a)
		| isCons a  = FConst (tan (sacarNum (a)))
		| otherwise = FTan (eval a)
eval (FCot a)
		| isCons a  = FConst (1/tan(sacarNum (a)))
		| otherwise = FCot (eval a)
eval (FSec a)
		| isCons a  = FConst (1/cos (sacarNum (a)))
		| otherwise = FSec (eval a)
eval (FCsc a)
		| isCons a  = FConst (1/sin (sacarNum (a)))
		| otherwise = FCsc (eval a)

{-Funcion que determina hasta que punto se reduce
-}
reduccion :: Func -> Func
reduccion t = let t' = eval t
              in if t == t'
                 then t'
                 else reduccion t'

--Función para ver si hay cambio de signo en el intervalo
signo f x y 
          |(reduccion (FMult (sust f ('x',x)) (sust f ('x',y)))) < (FConst 0) = True
          | otherwise = False                                                             

-- Función que me dice si es raiz
raiz :: Func -> Func -> Bool         
raiz f x 
     | reduccion (sust f ('x',x)) == FConst 0  = True
     | otherwise = False

-- Función que realiza la búsqueda incremental de un intervalo que contenga almenos 1 raiz
busqdIncremental :: Func -> Func -> Func -> Integer -> [(Func,Func)]
busqdIncremental f a d 0 = []
busqdIncremental f a d i 
    | (raiz f a /= False) = [((reduccion a),(reduccion a))] 
    | (signo f a s@(FSum a d) /= False) = [((reduccion a),(reduccion s)] 
    | (otherwise) = (busqdIncremental f (FSum a d) d (i-1))