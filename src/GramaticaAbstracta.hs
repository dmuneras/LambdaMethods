module GramaticaAbstracta where

{-Func representa un pequeño lenguaje para la gramatica de las funciones.
-}
data Func = FConst Double  	--Una constante
	  | FVar Char		--Una variable
	  | FSum Func Func	--Suma de funciones 
	  | FRes Func Func	--Resta de funciones
	  | FMult Func Func	--Multiplicacion de funciones
	  | FDiv Func Func	--Division de funciones
	  | FPot Func Func	--Potencia de funciones
	  | FExp Func		--Funcion exponencial
	  | FLn Func		--Funcion logaritmica
	  | FSen Func		--Funcion Seno
	  | FCos Func		--Funcion Coseno
	  | FTan Func		--Funcion Tangente
	  | FSec Func		--Funcion Secante
	  | FCsc Func		--Funcion Cosecante
	  | FCot Func		--Funcion Cotangente
          | Func Func           --Funcion que recibe cualquier de las demas funciones
	deriving (Eq,Ord)

data Resp = RSim Func           -- Una respuesta simple, se utiliza cuando se encuentra la un valor exacto como resultado.
          | RInt (Func,Func)    -- Respuesta como intervalo
        deriving (Show,Eq,Ord)  

instance Show Func where
    show (FConst a) = (show a)
    show (FVar x) = [x]
    show (FSum a b) = (show a) ++ "+" ++ (show b)
    show (FRes a b) = (show a) ++ "-" ++ (show b)
    show (FMult a b) = (show a) ++ "*" ++ (show b)
    show (FDiv a b) = (show a) ++ "/" ++ (show b)
    show (FPot a b) = (show a) ++"^" ++ "(" ++ (show b) ++ ")"
    show (FExp a) = "e^(" ++ (show a) ++ ")"
    show (FLn a) = "LN(" ++ (show a) ++ ")"
    show (FSen a) = "Sen(" ++ (show a) ++ ")"
    show (FCos a) = "Cos(" ++ (show a) ++ ")"
    show (FTan a) = "Tan(" ++ (show a) ++ ")"
    show (FSec a) = "Sec(" ++ (show a) ++ ")"
    show (FCsc a) = "Csc(" ++ (show a) ++ ")"
    show (FCot a) = "Cot(" ++ (show a) ++ ")"

{-instance Num  Func  where 
    (FConst ( a)) + (FConst ( b)) = (FConst (a+b)) 
    (FConst a) - (FConst b) = (FConst (a-b))
    (FConst a) * (FConst b) = (FConst (a*b))                            
    signum (FConst a) = FConst (signum a)
    abs (FConst a) = abs (FConst a) 
    fromInteger  a =  (FConst ((fromInteger a)))
 
instance Fractional Func where
    (FConst a)/ (FConst b) = FConst ((a/b))
    recip (FConst a) = FConst (recip a)
    fromRational a = FConst (fromRational a)-} 
   