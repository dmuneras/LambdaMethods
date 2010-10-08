module FuncionesInterfaz where

import Semantica
import GramaticaConcreta
import GramaticaAbstracta
import EcuacionesNoLineales
import GraficosFunciones
import UU.Parsing
import Graphics.UI.Gtk
import Prelude
import Control.Exception
import Foreign

process ::(EntryClass e) => String -> String -> String -> String -> String -> String -> e -> IO ()
process f a b tol i p e  
              | (p == "ver ecuacion parser ") = do a <- parseIO pFunc (funScanTxt f)
                                                   let st = show a
                                                   set e [ entryText := show st ]
              | (p == "Busqueda incremental ") = do f <- parseIO pFunc (funScanTxt f)
                                                    b <- parseIO pFunc (funScanTxt b)
                                                    a <-parseIO pFunc (funScanTxt a)
                                                    putStrLn ((show a)++ " incrementando " ++ (show b) ++ " iter " ++ (show i))
                                                    let st = busqdIncremental f a b (read i)
                                                    set e [entryText := show st]
              | (p == "Biseccion ") = do f <- parseIO pFunc (funScanTxt f)
                                         b <- parseIO pFunc (funScanTxt b)
                                         a <-parseIO pFunc (funScanTxt a)
                                         tol <- parseIO pFunc (funScanTxt tol)
                                         putStrLn ("Biseccion "++"Intervalo "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                         let st = biseccion f a b tol (read i) "abs"
                                         set e [entryText := st]
             | (p == "Regla falsa ") = do f <- parseIO pFunc (funScanTxt f)
                                          b <- parseIO pFunc (funScanTxt b)
                                          a <-parseIO pFunc (funScanTxt a)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("Regla falsa "++"Intervalo "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                          let st = reglaFalsa f a b tol (read i) "abs"
                                          set e [entryText := st]
              | (p == "Punto Fijo ") = do f <- parseIO pFunc (funScanTxt f)
                                          g <- parseIO pFunc (funScanTxt a)
                                          a <-parseIO pFunc (funScanTxt b)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("puntof"++"Inicial "++(show a) ++ " f'" ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                          let st = puntoFijo f a g tol (read i) "abs"
                                          set e [entryText := st]
              | otherwise = set e [entryText := "todavia no"]


{- Funcion que me retorna el texto utilizado como control en el process-}
control :: RadioButton -> String
control b =  unsafePerformIO(get b buttonLabel)

{-Funcion que me retorna el radiobutton activo-}
controlRadio :: [RadioButton] -> RadioButton
controlRadio ar = head (filter (\x -> unsafePerformIO(toggleButtonGetActive x)) ar)   
  

{-Funcion que va indicando en la consola el cambio de estado de los radiobutton -}
setRadioState :: RadioButton -> IO ()
setRadioState b = do
  state <- toggleButtonGetActive b
  label <- get b buttonLabel
  putStrLn ("State " ++ label ++ " now is " ++ (show state))  

            

   
 
  

 
  