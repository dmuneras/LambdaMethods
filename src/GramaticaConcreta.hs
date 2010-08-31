module GramaticaConcreta where

import UU.Parsing
import Data.Char
import Data.List
import UU.Scanner
import UU.Scanner.GenTokenParser
import GramaticaAbstracta


kwtxt  = ["D", "E", "LN", "Sen", "Cos", "Tan", "Cot", "Sec", "Csc"]
kwotxt = ["|", "+", "-", "*", "/", "^", ".", "==","!=", ">=","<=", ">","<"]
sctxt  = "(),"
octxt  = "|+*-/^."

funScan = scan kwtxt kwotxt sctxt octxt 

funScanFl :: FilePath -> String -> [Token]
funScanFl f s = funScan (Pos 1 0 f) s

funScanTxt :: String -> [Token]
funScanTxt = funScan (Pos 0 0 "") 

{-Parser que toma una cadena de digitos de un String y la convierte a double
-}
pDouble :: Parser Token Double
pDouble = read <$> pInteger
	<|> g  <$> pKey "-" <*> pInteger
	<|> f  <$> pInteger <*> pKey "." <*> pInteger
	<|> h  <$> pKey "-" <*> pInteger <*> pKey "." <*> pInteger
	where f a p b  = read (a ++ p ++ b)
	      g m a  = read (m ++ a)
	      h m a p b = read (m ++ a ++ p ++ b)	
	      				
{-Parser que reconoce un numero de doble prescisión y lo convierte a funcion constante
-}
pConst :: Parser Token Func
pConst = FConst <$> pDouble 

{-Parser que reconoce un caracter comprendido entre 'a' y 'z' y lo convierte
a variable
-}
pVar :: Parser Token Func
pVar = FVar <$> (head <$> pVarid)

{-Parser que reconoce un factor
-}
pFactor :: Parser Token Func
pFactor = pConst
      <|> pVar
      <|> pFunExpon
      <|> pFunLn
      <|> pTrig
      <|> (pParens pFunc)

{-Parser que reconoce los operadores de multiplicacion y division
-}    

pOperMult :: Parser Token (Func -> Func -> Func)
pOperMult =  FMult <$ pKey "*"
	<|>  FDiv  <$ pKey "/"
	<|>  FPot  <$ pKey "^" 

{-Parser que reconoce los operadores de suma y resta
-}
pOperSum :: Parser Token (Func -> Func -> Func)
pOperSum =  FSum <$ pKey "+"
        <|> FRes <$ pKey "-"
    
{-Parser que reconoce un termino
-}
pTerm :: Parser Token Func
pTerm = pChainl pOperMult pFactor

{-Parser que reconoce una función en forma de expresion aritmética y lo convierte
a la funcion respectiva
-}
pFunExpression :: Parser Token Func
pFunExpression = pChainl pOperSum pTerm

{-Parser que reconoce la funcion exponencial
-}
pFunExpon :: Parser Token Func
pFunExpon = FExp <$>(pKey "E" *> pFunc)

{-Parser que reconoce la funcion logaritmo natural
-}
pFunLn :: Parser Token Func
pFunLn = FLn <$> (pKey "LN" *> pFunc)

{-Parser que reconoce la funcion Seno
-}
pSen :: Parser Token Func
pSen = FSen  <$> (pKey "Sen" *> pFunc)

{-Parser que reconoce la funcion Coseno
-}	
pCos :: Parser Token Func
pCos = FCos  <$> (pKey "Cos" *> pFunc)

{-Parser que reconoce la funcion Tangente
-}
pTan :: Parser Token Func
pTan = FTan  <$> (pKey "Tan" *> pFunc)

{-Parser que reconoce la funcion Secante
-}
pSec :: Parser Token Func
pSec = FSec <$> (pKey "Sec" *> pFunc)

{-Parser que reconoce la funcion Cosecante
-}
pCsc :: Parser Token Func
pCsc = FCsc <$> (pKey "Csc" *> pFunc)

{-Parser que reconoce la funcion Cotangente
-}
pCot :: Parser Token Func
pCot = FCot <$> (pKey "Cot" *> pFunc)

{-Parser que reconoce una funcion trigonometrica
-}
pTrig :: Parser Token Func
pTrig = pSen <|> pCos <|> pTan <|> pCot <|> pSec <|> pCsc

{-Parser que reconoce una funcion
-}	
pFunc :: Parser Token Func
pFunc = pConst <|> pVar <|> pFunExpression <|> pFunExpon <|> pFunLn <|> pTrig

{-Parser que reconoce una tupla sin parentesis
-}
pTupla :: Parser Token (Double,Double)
pTupla = f <$> pDouble <* pComma <*> pDouble
         where f a b = (a,b)

--instance Eq Func where
--    FConst f == FConst a =  f == a
--    FConst _ == FVar _ =  False 
--    FVar _ == FConst _ =  False
--    FVar x   == FVar y   =  x == y 

     
    

