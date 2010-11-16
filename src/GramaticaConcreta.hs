module GramaticaConcreta where

import UU.Parsing
import Data.Char
import Data.List
import UU.Scanner
import UU.Scanner.GenTokenParser
import GramaticaAbstracta

kwtxt  = ["E", "EXP", "LN", "Sen", "Cos", "Tan", "Cot", "Sec", "Csc"]
kwotxt = ["|", "+", "-", "*", "/", "^", "."]
sctxt  = "(),"
octxt  = "|+*-/^."

funScan = scan kwtxt kwotxt sctxt octxt 

funScanFl :: FilePath -> String -> [Token]
funScanFl f s = funScan (Pos 1 0 f) s

funScanTxt :: String -> [Token]
funScanTxt = funScan (Pos 0 0 "") 

{-Parser que reconoce un entero negativo
-}
pIntegerSign :: Parser Token String
pIntegerSign =  pInteger
            <|> f <$> pKey "-" <*> pInteger
                where f m a = (m ++ a)

{-Parser que reconoce un numero con parte entera y parte real
-}
pNumReal :: Parser Token String
pNumReal = f <$> pIntegerSign <*> pKey "." <*> pInteger
           where f a p b = (a ++ p ++ b)

{-Parser que toma una cadena de digitos de un String y la convierte a double
-}
pDouble :: Parser Token Double
pDouble = read <$> pIntegerSign
        <|> read  <$> pNumReal
        <|> f  <$> pIntegerSign <*> pKey "E" <*> pIntegerSign
        <|> f  <$> pNumReal <*> pKey "E" <*> pIntegerSign
        <|> g  <$> pKey "E" <*> pIntegerSign
        <|> h  <$> pKey "-E" <*> pIntegerSign
	where f a e ex = read (a ++ "e" ++ ex)
              g e ex = read ("1e" ++ ex)
              h e ex = read ("-1e" ++ ex)

{-Parser que reconoce una lista de doubles
-}
pListDouble :: Parser Token [Double]
pListDouble = pList pDouble
                  				
{-Parser que reconoce un numero y lo convierte a funcion constante
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
      <|> (pParens pFactor)

-- pFactorP :: Parser Token Func
-- pFactorP = (pParens pFactor)

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

{-Parser que reconoce una funcion de forma de expresion aritmética y lo convierte
a la funcion respectiva
-}
pFunExpression :: Parser Token Func
pFunExpression = pChainl pOperSum pTerm

{-Parser que reconoce la funcion exponencial
-}
pFunExpon :: Parser Token Func
pFunExpon = FExp <$>(pKey "EXP" *> (pParens pFunc))

{-Parser que reconoce la funcion logaritmo natural
-}
pFunLn :: Parser Token Func
pFunLn = FLn <$> (pKey "LN(" *> (pParens pFunc))

{-Parser que reconoce la funcion Seno
-}
pSen :: Parser Token Func
pSen = FSen  <$> (pKey "Sen" *> (pParens pFunc))

{-Parser que reconoce la funcion Coseno
-}	
pCos :: Parser Token Func
pCos = FCos  <$> (pKey "Cos" *> (pParens pFunc))

{-Parser que reconoce la funcion Tangente
-}
pTan :: Parser Token Func
pTan = FTan  <$> (pKey "Tan" *> (pParens pFunc))

{-Parser que reconoce la funcion Secante
-}
pSec :: Parser Token Func
pSec = FSec <$> (pKey "Sec" *>  (pParens pFunc))

{-Parser que reconoce la funcion Cosecante
-}
pCsc :: Parser Token Func
pCsc = FCsc <$> (pKey "Csc" *>  (pParens pFunc))

{-Parser que reconoce la funcion Cotangente
-}
pCot :: Parser Token Func
pCot = FCot <$> (pKey "Cot" *>  (pParens pFunc))

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
