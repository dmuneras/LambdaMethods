module InterfazInterpolacion where

import FuncionesInterfaz
import Interpolacion
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign

interpolacion :: IO Table
interpolacion =  do 
                   table <- tableNew 9 1 False
                   tableSetColSpacings table 10
                   content <- vBoxNew False 0
                   datosx <- hBoxNew False 5
                   datosy <- hBoxNew False 3
                   num <- hBoxNew False 5
                   poli <- hBoxNew False 5
                   sali <- hBoxNew False 5
                   tableAttachDefaults table content 0 1 0 1

                   labelx <- labelNew  (Just "Ingrese los valores de las x separados por espacios")
                   puntosx <- entryNew
                   boxPackStart datosx labelx PackNatural 5
                   boxPackStart datosx puntosx PackNatural 5
                   boxPackStart content datosx PackNatural 5

                   labely <- labelNew  (Just "Ingrese los valores de los f(x) separados por espacios")             
                   puntosy <- entryNew
                   boxPackStart datosy labely  PackNatural 5
                   boxPackStart datosy puntosy PackNatural 5
                   boxPackStart content datosy PackNatural 5

                   labelvx <- labelNew  (Just "Ingrese el valor en donde desea evaluar el polinomio")   
                   valorx <- entryNew
                   boxPackStart num labelvx  PackNatural 5
                   boxPackStart num valorx PackNatural 5
                   boxPackStart content num PackNatural 5

                   labelsali <- labelNew  (Just "Valor interpolado: ")
                   salida  <- entryNew
                   boxPackStart sali labelsali PackNatural 5
                   boxPackStart sali salida  PackNatural 0
                   boxPackStart content sali PackNatural 0
                   
                   labelpoli <- labelNew  (Just "Polinomio interpolante: ")
                   polinomio <- entryNew
                   boxPackStart content labelpoli  PackNatural 0
                   boxPackStart content polinomio PackNatural 0
 
                   
                   eval <- buttonNewWithLabel "Evaluar"
                   boxPackStart content eval  PackNatural 0
                   graficar <- buttonNewWithLabel "Graficar"
                   boxPackStart content graficar  PackNatural 0
                   ayuda <- buttonNewWithLabel "Necesitas ayuda?"
                   boxPackStart content ayuda  PackNatural 0

                   {-NEWTON-}
                   newtoninter <- hBoxNew False 25
                   option <- vBoxNew False 0
                   tableAttachDefaults table newtoninter 0 1 2 3
                   radio1 <- radioButtonNewWithLabel "Interpolar con Newton "
                   boxPackStart option radio1 PackNatural 5
                   boxPackStart newtoninter option PackNatural 0

                   {-LAGRANGE-}
                   lagrange <- hBoxNew False 25
                   option <- vBoxNew False 0
                   tableAttachDefaults table  lagrange 0 1 3 4
                   radio2 <- radioButtonNewWithLabelFromWidget radio1 "Interpolar con Lagrange "
                   boxPackStart option radio2 PackNatural 5
                   boxPackStart lagrange option PackNatural 0
                   
                   {-LINEALES-}
                   linealinter <- hBoxNew False 25
                   option <- vBoxNew False 0
                   tableAttachDefaults table linealinter 0 1 4 5
                   radio3 <-radioButtonNewWithLabelFromWidget radio2 "Interpolar con Trazadores Lineales "
                   boxPackStart option radio3 PackNatural 5
                   boxPackStart linealinter option PackNatural 0
                   
                   
                   {-CUADRATICOS-}
                   cuadraticointer <- hBoxNew False 25
                   option <- vBoxNew False 0
                   tableAttachDefaults table cuadraticointer 0 1 5 6
                   radio4 <- radioButtonNewWithLabelFromWidget radio3 "Interpolar con Trazadores Cuadráticos "
                   boxPackStart option radio4 PackNatural 5
                   boxPackStart cuadraticointer option PackNatural 0
                 
                    
                   {-CUBICO-}
                   cubicointer <- hBoxNew False 25
                   option <- vBoxNew False 0
                   tableAttachDefaults table cubicointer 0 1 6 7
                   radio5 <- radioButtonNewWithLabelFromWidget radio4 " Interpolar con Trazadores Cúbicos"
                   boxPackStart option radio5 PackNatural 5
                   boxPackStart cubicointer option PackNatural 0
                   
                   onClicked eval $ do
                                      px <- get puntosx entryText 
                                      listx <- parseIO pListDouble (funScanTxt px)
                                      py <- get puntosy entryText 
                                      listy <- parseIO pListDouble (funScanTxt py)
                                      v <- get valorx entryText 
                                      valorx <- parseIO pDouble (funScanTxt v)
                                      if (unsafePerformIO(toggleButtonGetActive radio1))
                                       then do
                                              let result = newtonEvaluado listx listy valorx
                                              let poliresult = newtonPolinomio listx listy
                                              set salida [entryText := show(result)]
                                              set polinomio [entryText := show(poliresult)]
                                       else
                                           if (unsafePerformIO(toggleButtonGetActive radio2))
                                            then do
                                                   valorx <- parseIO pDouble (funScanTxt v)
                                                   let result = lagrangeEvaluado listx listy valorx
                                                   let poliresult = lagrangePolinomio listx listy
                                                   set salida [entryText := show(result)]
                                                   set polinomio [entryText := show(poliresult)]
                                            else
                                               if (unsafePerformIO(toggleButtonGetActive radio3))
                                                then do
                                                       valorx <- parseIO pDouble (funScanTxt v)
                                                       let result = spLinEvaluado listx listy valorx
                                                       let polinomios = spLinEcuaciones listx listy
                                                       set polinomio [entryText := show(polinomios)]
                                                       set salida [entryText := show(result)]
                                                 else
                                                     if (unsafePerformIO(toggleButtonGetActive radio4))
                                                      then do
                                                             valorx <- parseIO pDouble (funScanTxt v)
                                                             let result = lagrangeEvaluado listx listy valorx
                                                             set salida [entryText := show(result)]
                                                     else do
                                                             valorx <- parseIO pDouble (funScanTxt v)
                                                             let result = lagrangeEvaluado listx listy valorx
                                                             set salida [entryText := show(result)]
                                                       
                                      
                          
                   
                   return table

                          