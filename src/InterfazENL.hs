module InterfazENL where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign

ecuacionesNoLineales :: IO Table
ecuacionesNoLineales = do table <- tableNew 6 1 False
                        
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
                          radio1 <- radioButtonNewWithLabel "ver ecuacion parser "
                          boxPackStart show radio1 PackNatural 5
                          sep <- hSeparatorNew
                          boxPackStart show sep PackNatural 0

                          {-BUSQUEDAS INCREMENTALES-}

                          busqdInc <- hBoxNew False 0 
                          option <- vBoxNew False 0
                          ba <- vBoxNew False 0
                          bb <- vBoxNew False 0 
                          bi <- vBoxNew False 0
                          tableAttachDefaults table busqdInc 0 1 2 3
                          radio2 <- radioButtonNewWithLabelFromWidget radio1 "Busqueda incremental "
                          boxPackStart option radio2 PackNatural 5
                          boxPackStart busqdInc option PackNatural 0
                          boxPackStart busqdInc ba PackNatural 0
                          boxPackStart busqdInc bb PackNatural 0
                          boxPackStart busqdInc bi PackNatural 0
  
   
                          labela <- labelNew (Just "ingrese valor inicial  ")
                          miscSetAlignment labela 0 0
                          boxPackStart ba labela PackNatural 0
                          a <- entryNew
                          boxPackStart ba a PackNatural 0

                          labelb <- labelNew (Just "ingrese incremento ")
                          miscSetAlignment labelb 0 0
                          boxPackStart bb labelb PackNatural 0
                          b <- entryNew 
                          boxPackStart bb b PackNatural 0
  
                          labeli <- labelNew (Just "ingrese numero de iteraciones ")
                          miscSetAlignment labeli 0 0
                          boxPackStart bi labeli PackNatural 0
                          i <- entryNew
                          boxPackStart bi i PackNatural 0
   
                          {-BISECCION-}
  
                          biseccion <- hBoxNew False 0 
                          optionb <- vBoxNew False 0
                          bab <- vBoxNew False 0
                          bbb <- vBoxNew False 0 
                          btolb<- vBoxNew False 0
                          bib <- vBoxNew False 0
                           
                          tableAttachDefaults table biseccion 0 1 3 4
                          radio3 <- radioButtonNewWithLabelFromWidget radio2 "Biseccion "
                          boxPackStart biseccion optionb PackNatural 0
                          boxPackStart optionb radio3 PackNatural 5
                          boxPackStart biseccion bab PackNatural 0
                          boxPackStart biseccion bbb PackNatural 0
                          boxPackStart biseccion btolb PackNatural 0
                          boxPackStart biseccion bib PackNatural 0
  
                          labelab     <- labelNew (Just "ingrese valor de a ")
                          miscSetAlignment labelab 0 0
                          boxPackStart bab labelab PackNatural 0
                          ab <- entryNew
                          boxPackStart bab ab PackNatural 0
  
                          labelbb <- labelNew (Just "ingrese valor de b ")
                          miscSetAlignment labelbb 0 0
                          boxPackStart bbb labelbb PackNatural 0
                          bb <- entryNew 
                          boxPackStart bbb bb PackNatural 0
   
                          labeltolb     <- labelNew (Just "ingrese tolerancia ")
                          miscSetAlignment labeltolb 0 0
                          boxPackStart btolb labeltolb PackNatural 0
                          tol <- entryNew
                          boxPackStart btolb tol PackNatural 0

                          labelib     <- labelNew (Just "ingrese iteraciones ")
                          miscSetAlignment labelib 0 0
                          boxPackStart bib labelib PackNatural 0
                          ib <- entryNew
                          boxPackStart bib ib PackNatural 0
                          return table
                          {-REGLAFALSA-}
  
                          reglaFalsa <- hBoxNew False 0 
                          optionr <- vBoxNew False 0
                          bar <- vBoxNew False 0
                          bbr <- vBoxNew False 0 
                          btolr<- vBoxNew False 0
                          bir <- vBoxNew False 0
 
                          tableAttachDefaults table reglaFalsa 0 1 4 5
                          radio4 <- radioButtonNewWithLabelFromWidget radio3 "Regla falsa "
                          boxPackStart reglaFalsa optionr PackNatural 0
                          boxPackStart optionr radio4 PackNatural 5
                          boxPackStart reglaFalsa bar PackNatural 0
                          boxPackStart reglaFalsa bbr PackNatural 0
                          boxPackStart reglaFalsa btolr PackNatural 0
                          boxPackStart reglaFalsa bir PackNatural 0
  
                          labelar <- labelNew (Just "ingrese valor de a ")
                          miscSetAlignment labelar 0 0
                          boxPackStart bar labelar PackNatural 0
                          ar <- entryNew
                          boxPackStart bar ar PackNatural 0
  
                          labelbr <- labelNew (Just "ingrese valor de b ")
                          miscSetAlignment labelbr 0 0
                          boxPackStart bbr labelbr PackNatural 0
                          br <- entryNew 
                          boxPackStart bbr br PackNatural 0
   
                          labeltolr     <- labelNew (Just "ingrese tolerancia ")
                          miscSetAlignment labeltolr 0 0
                          boxPackStart btolr labeltolr PackNatural 0
                          tolr <- entryNew
                          boxPackStart btolr tolr PackNatural 0

                          labelir <- labelNew (Just "ingrese iteraciones ")
                          miscSetAlignment labelir 0 0
                          boxPackStart bir labelir PackNatural 0
                          ir <- entryNew
                          boxPackStart bir ir PackNatural 0
                          
                          {-PUNTO FIJO-}
                          puntoFijo <- hBoxNew False 0 
                          optionp <- vBoxNew False 0
                          bap <- vBoxNew False 0
                          bbp <- vBoxNew False 0 
                          btolp<- vBoxNew False 0
                          bip <- vBoxNew False 0
 
                          tableAttachDefaults table puntoFijo 0 1 5 6
                          radio5 <- radioButtonNewWithLabelFromWidget radio4 "Punto Fijo "
                          boxPackStart puntoFijo optionp PackNatural 0
                          boxPackStart optionp radio5 PackNatural 5
                          boxPackStart puntoFijo bap PackNatural 0
                          boxPackStart puntoFijo bbp PackNatural 0
                          boxPackStart puntoFijo btolp PackNatural 0
                          boxPackStart puntoFijo bip PackNatural 0
  
                          labelap <- labelNew (Just "ingrese valor inicial ")
                          miscSetAlignment labelap 0 0
                          boxPackStart bap labelap PackNatural 0
                          ap <- entryNew
                          boxPackStart bap ap PackNatural 0
  
                          labelbp <- labelNew (Just "ingrese función g ")
                          miscSetAlignment labelbp 0 0
                          boxPackStart bbp labelbp PackNatural 0
                          bp <- entryNew 
                          boxPackStart bbp bp PackNatural 0
   
                          labeltolp     <- labelNew (Just "ingrese tolerancia ")
                          miscSetAlignment labeltolp 0 0
                          boxPackStart btolp labeltolp PackNatural 0
                          tolp <- entryNew
                          boxPackStart btolp tolp PackNatural 0

                          labelip <- labelNew (Just "ingrese iteraciones ")
                          miscSetAlignment labelip 0 0
                          boxPackStart bip labelip PackNatural 0
                          ip <- entryNew
                          boxPackStart bip ip PackNatural 0

                          
                          {-FUNCIONES PARA CONTROLAR LOS EVENTOS-}
                          toggleButtonSetActive radio1 True
                          onToggled radio1 (setRadioState radio1)
                          onToggled radio2 (setRadioState radio2)
                          onToggled radio3 (setRadioState radio3)
                          onToggled radio4 (setRadioState radio4)
                          onToggled radio4 (setRadioState radio5)

                          {-CAPTURA DE EVENTO DEL GRAFICADOR-}
                          onClicked graficar $ do
                                s <- get entrada entryText
                                f <- parseIO pFunc (funScanTxt s)
                                graficaXY f
 
                          onClicked eval $ do
                                f <- get entrada entryText
                                if (unsafePerformIO(toggleButtonGetActive radio5))
                                 then do
                                        ap <- get ap entryText
                                        bp <- get bp entryText
                                        tolp <- get tolp entryText
                                        ip <- get ip entryText
                                        process f ap bp tolp ip (control(controlRadio [radio1,radio2, radio3, radio4,radio5])) salida
                                 else        
                                     if (unsafePerformIO(toggleButtonGetActive radio4))
                                      then do
                                             ar <- get ar entryText
                                             br <- get br entryText
                                             tolr <- get tolr entryText
                                             ir <- get ir entryText
                                             process f ar br tolr ir (control(controlRadio [radio1,radio2, radio3, radio4,radio5])) salida
                                      else
                                          if (unsafePerformIO(toggleButtonGetActive radio3))
                                           then do
                                                  ab <- get ab entryText
                                                  bb <- get bb entryText
                                                  tol <- get tol entryText
                                                  ib <- get ib entryText
                                                  process f ab bb tol ib (control(controlRadio [radio1,radio2, radio3,radio4,radio5])) salida
                                            else do
                                                   a <- get a entryText
                                                   b <- get b entryText
                                                   i <- get i entryText
                                                   process f a b i i (control(controlRadio [radio1,radio2, radio3,radio4,radio5])) salida
 
  
                          return table                                       
                                   
          