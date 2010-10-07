module Main where

import Semantica
import GramaticaConcreta
import GramaticaAbstracta
import EcuacionesNoLineales
import GraficosFunciones
import UU.Parsing
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Prelude
import Control.Exception
import Foreign

process ::(EntryClass e) => String -> String -> String -> String -> String -> String -> e -> IO ()
process f a b tol i p e  
              | (p == "ver ecuacion parser") = do a <- parseIO pFunc (funScanTxt f)
                                                  let st = show a
                                                  set e [ entryText := show st ]
              | (p == "Busqueda incremental") = do f <- parseIO pFunc (funScanTxt f)
                                                   b <- parseIO pFunc (funScanTxt b)
                                                   a <-parseIO pFunc (funScanTxt a)
                                                   putStrLn ((show a)++ " incrementando " ++ (show b) ++ " iter " ++ (show i))
                                                   let st = busqdIncremental f a b (read i)
                                                   set e [entryText := show st]
              | (p == "Biseccion") = do f <- parseIO pFunc (funScanTxt f)
                                        b <- parseIO pFunc (funScanTxt b)
                                        a <-parseIO pFunc (funScanTxt a)
                                        tol <- parseIO pFunc (funScanTxt tol)
                                        putStrLn ("Intervalo "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                        let st = biseccion f a b tol (read i) "abs"
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

            
{-Funcion Principal para la creacion de la interfaz del proyecto-}
main = do

  {-PANEL PRINCIPAL-}
  initGUI
  ventana     <- windowNew
  set ventana [windowTitle := "LambdaMethods",
              containerBorderWidth := 5,  windowDefaultWidth := 300,
              windowDefaultHeight := 400 ]
  table   <- tableNew 3 1 True
  containerAdd ventana table

  content <- vBoxNew False 0
  tableAttachDefaults table content 0 1 0 1
  entrada <- entryNew
  boxPackStart content entrada PackNatural 5
  salida  <- entryNew
  boxPackStart content salida PackNatural 5
  eval <- buttonNewWithLabel "Evaluar"
  boxPackStart content eval  PackNatural 0
  graficar <- buttonNewWithLabel "Graficar"
  boxPackStart content graficar  PackNatural 0
  
  {-SHOW DEL RESULTADO DEL PARSER-}
  show  <- vBoxNew False 0
  tableAttachDefaults table show 0 1 1 2
  radio1 <- radioButtonNewWithLabel "ver ecuacion parser"
  boxPackStart show radio1 PackNatural 0

  {-BUSQUEDAS INCREMENTALES-}

  busqdInc <- hBoxNew False 0 
  tableAttachDefaults table busqdInc 0 1 1 2
  radio2 <- radioButtonNewWithLabelFromWidget radio1 "Busqueda incremental"
  boxPackStart busqdInc radio2 PackNatural 0
  a <- entryNew
  boxPackStart busqdInc a PackNatural 0
  b <- entryNew 
  boxPackStart busqdInc b PackNatural 0
  i <- entryNew
  boxPackStart busqdInc i PackNatural 0
  
  {-BISECCION-}
  
  biseccion <- hBoxNew False 0 
  tableAttachDefaults table biseccion 0 1 2 3
  radio3 <- radioButtonNewWithLabelFromWidget radio2 "Biseccion"
  boxPackStart biseccion radio3 PackNatural 0
  ab <- entryNew
  boxPackStart biseccion ab PackNatural 0
  bb <- entryNew 
  boxPackStart biseccion bb PackNatural 0
  tol <- entryNew
  boxPackStart biseccion tol PackNatural 0
  ib <- entryNew
  boxPackStart biseccion ib PackNatural 0
  
  {-FUNCIONES PARA CONTROLAR LOS EVENTOS-}

  toggleButtonSetActive radio1 True
  onToggled radio1 (setRadioState radio1)
  onToggled radio2 (setRadioState radio2)
  onToggled radio3 (setRadioState radio3)
  
  -- onEntryActivate entrada $ do
  --       texto <- get entrada entryText
  --       if (unsafePerformIO(toggleButtonGetActive radio2)) 
  --         then do 
  --                a <- get a entryText 
  --                b <- get b entryText
  --                i <- get i entryText
  --                process texto salida a b i i (control(controlRadio [radio1,radio2, radio3]))
  --         else do
  --                ab <- get a entryText
  --                bb <- get b entryText
  --                ib <- get i entryText
  --                tol <- get tol entryText
  --                process texto salida ab bb tol ib (control(controlRadio [radio1,radio2, radio3]))

  onClicked eval $ do
        f <- get entrada entryText
        if (unsafePerformIO(toggleButtonGetActive radio3)) 
          then do 
                 ab <- get ab entryText 
                 bb <- get bb entryText
                 tol <- get tol entryText
                 ib <- get ib entryText
                 process f ab bb tol ib (control(controlRadio [radio1,radio2, radio3])) salida
          else do
                 a <- get a entryText
                 b <- get b entryText
                 i <- get i entryText
                 process f a b i i (control(controlRadio [radio1,radio2, radio3])) salida
  
  
  {-Captura evento del boton graficar-}
  onClicked graficar $ do
        s <- get entrada entryText
        f <- parseIO pFunc (funScanTxt s)
        graficaXY f
  
 
 
  onDestroy ventana mainQuit
  widgetShowAll ventana
  mainGUI
   
  

 
  