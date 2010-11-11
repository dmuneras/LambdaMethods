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

ayudaENL :: IO()
ayudaENL =  do
     initGUI
     windowAyuda <- windowNew
     set windowAyuda [windowTitle := "Ayudas Ecuaciones no lineales", windowDefaultWidth := 400,
                 windowDefaultHeight := 400 ]

     tableAyuda <- tableNew 2 2 True
     containerAdd windowAyuda tableAyuda
            
     text <- labelNew (Just "Seleccione el tema sobre el que necesita ayuda.")
     tableAttachDefaults tableAyuda text 0 1 0 1
     
  
     radios <- vBoxNew False 0 
     optionabi <- vBoxNew False 0
     optionab <- vBoxNew  False 0
     optionrf <- vBoxNew  False 0
     optionpf <- vBoxNew  False 0
     tableAttachDefaults tableAyuda radios 0 1 1 2
     radioabi <- radioButtonNewWithLabel "Busqueda incremental "
     radioab <- radioButtonNewWithLabelFromWidget radioabi "Biseccion "
     radiorf <- radioButtonNewWithLabelFromWidget radioab "Regla falsa "
     radiopf <- radioButtonNewWithLabelFromWidget radiorf "Punto fijo "
     boxPackStart optionabi radioabi PackNatural 5
     boxPackStart optionab radioab PackNatural 5
     boxPackStart optionrf radiorf PackNatural 5
     boxPackStart optionpf radiopf PackNatural 5
     boxPackStart radios optionabi PackNatural 0
     boxPackStart radios optionab PackNatural 0
     boxPackStart radios optionrf PackNatural 0
     boxPackStart radios optionpf PackNatural 0
     
     textAyuda <- labelNew Nothing
     tableAttachDefaults tableAyuda textAyuda 1 2 0 1
    

     onToggled radioabi $ do
               labelSetText textAyuda "Búsqueda Incremental"
               setRadioState radioabi
     onToggled radioab $ do
              labelSetText textAyuda (control(controlRadio [radioabi, radioab,radiorf,radiopf]))
              setRadioState radioab
     onToggled radiorf $ do
              labelSetText textAyuda (control(controlRadio [radioabi, radioab,radiorf,radiopf]))
              setRadioState radiorf
     onToggled radiopf $ do
              labelSetText textAyuda (control(controlRadio [radioabi, radioab,radiorf,radiopf]))
              setRadioState radiopf

    
     widgetShowAll windowAyuda
     onDestroy windowAyuda mainQuit
     mainGUI
      

   
 
  

 
  