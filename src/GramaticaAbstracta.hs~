module GramaticaAbstracta where

{-Defincion de la gramatica abstracta para escribir funciones e integrales.
-}

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
	deriving (Eq, Ord, Show)

-- instance Show Func where
--     show (FConst a) = (show a)
--     show (FVar x) = [x]
--     show (FSum a b) = (show a) ++ "+" ++ (show b)
--     show (FRes a b) = (show a) ++ "-" ++ (show b)
--     show (FMult a b) = (show a) ++ "*" ++ (show b)
--     show (FDiv a b) = (show a) ++ "/" ++ (show b)
--     show (FPot a b) = (show a) ++"^" ++ "(" ++ (show b) ++ ")"
--     show (FExp a) = "e^(" ++ (show a) ++ ")"
--     show (FLn a) = "LN(" ++ (show a) ++ ")"
--     show (FSen a) = "Sen(" ++ (show a) ++ ")"
--     show (FCos a) = "Cos(" ++ (show a) ++ ")"
--     show (FTan a) = "Tan(" ++ (show a) ++ ")"
--     show (FSec a) = "Sec(" ++ (show a) ++ ")"
--     show (FCsc a) = "Csc(" ++ (show a) ++ ")"
--     show (FCot a) = "Cot(" ++ (show a) ++ ")"
