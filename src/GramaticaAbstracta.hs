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
	deriving (Show,Eq,Ord)

