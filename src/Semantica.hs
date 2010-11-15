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
sust (FPar a) s@(var, f)   = sust a s 

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

{-Funcion que saca el valor absoluto de una funcion
-}
abs' :: Func -> Func
abs' (FConst c) = FConst (abs c)
abs' (FVar x)   = error "Valor absoluto de variable"
abs' (FSum a b) = FConst (abs ((sacarNum (a) + sacarNum (b))))
abs' (FRes a b) = FConst (abs ((sacarNum (a) - sacarNum (b))))
abs' (FMult a b)= FConst (abs ((sacarNum (a) * sacarNum (b))))
abs' (FDiv a b) = FConst (abs ((sacarNum (a) / sacarNum (b))))
abs' (FPot a b) = FConst (abs ((sacarNum (a) ** sacarNum (b))))
abs' (FExp a)   = FConst (abs (exp (sacarNum (a))))
abs' (FLn a)    = FConst (abs (log (sacarNum (a))))
abs' (FSen a)   = FConst (abs (sin (sacarNum (a))))
abs' (FCos a)   = FConst (abs (cos (sacarNum (a))))
abs' (FTan a)   = FConst (abs (tan (sacarNum (a))))
abs' (FSec a)   = FConst (abs (1/sin (sacarNum (a))))
abs' (FCsc a)   = FConst (abs (1/cos (sacarNum (a))))
abs' (FCot a)   = FConst (abs (1/tan (sacarNum (a))))

{-Funcion que reduce los terminos de una funcion y de sus subterminos
-}          
resolution :: Func -> Func
resolution (FConst c) = FConst c
resolution (FVar x)   = FVar x
resolution (FSum a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)+ sacarNum (b))
		| otherwise                =  FSum (resolution a) (resolution b)
resolution (FRes a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)- sacarNum (b))
		| otherwise                =  FRes (resolution a) (resolution b)
resolution (FMult a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)* sacarNum (b))
		| otherwise                =  FMult (resolution a) (resolution b)
resolution (FDiv a b)
		| (isCons a) && (isCons b) = FConst (sacarNum (a)/ sacarNum (b))
		| otherwise                = FDiv (resolution a) (resolution b)
resolution (FPot a b)
		| (isCons a) && (isCons b) = FConst ((sacarNum (a)**(sacarNum (b))))
		| otherwise                = FPot (resolution a) (resolution b)
resolution (FExp a)
		| isCons a  = FConst (exp (sacarNum (a)))
		| otherwise = FExp (resolution a)
resolution (FLn a)
		| isCons a  = FConst (log (sacarNum (a)))
		| otherwise = FLn (resolution a)
resolution (FSen a)
		| isCons a  = FConst (sin (sacarNum (a)))
		| otherwise = FSen (resolution a)
resolution (FCos a)
		| isCons a  = FConst (cos (sacarNum (a)))
		| otherwise = FCos (resolution a)
resolution (FTan a)
		| isCons a  = FConst (tan (sacarNum (a)))
		| otherwise = FTan (resolution a)
resolution (FCot a)
		| isCons a  = FConst (1/tan(sacarNum (a)))
		| otherwise = FCot (resolution a)
resolution (FSec a)
		| isCons a  = FConst (1/cos (sacarNum (a)))
		| otherwise = FSec (resolution a)
resolution (FCsc a)
		| isCons a  = FConst (1/sin (sacarNum (a)))
		| otherwise = FCsc (resolution a)

{-Funcion que determina hasta que punto se reduce
-}
reduccion :: Func -> Func
reduccion t = let t' = resolution t
              in if t == t'
                 then t'
                 else reduccion t'

{-Funcion que evalua una funcion en un punto dado
-}
eval :: Func -> (Char, Func) -> Func
eval f a@(v, c) = reduccion (sust f a)

{- Funciones de ayuda para copiar operaciones aritmeticas (Helpers) -}
ton :: Double -> Func
ton a = FConst a
   
tov :: Char -> Func
tov a = FVar a

(+/) :: Func -> Func -> Func
(+/) a b = FSum a b
               
(-/) :: Func -> Func -> Func
(-/) a b = FRes a b

(*/) :: Func -> Func -> Func
(*/) a b = FMult a b

(//) :: Func -> Func -> Func
(//) a b = FDiv a b

(^/) :: Func -> Func -> Func
(^/) a b = FPot a b

toexp :: Func -> Func
toexp a = FExp a

toln :: Func -> Func
toln a = FLn a

cos_ :: Func -> Func
cos_ a = FCos a

sen_ :: Func -> Func
sen_ a = FSen a

tan_ :: Func -> Func
tan_ a = FTan a

sec_ :: Func -> Func
sec_ a = FSec a

csc_ :: Func -> Func
csc_ a = FCsc a

cot_ :: Func -> Func
cot_ a = FCot a