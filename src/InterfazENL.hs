module InterfazENL where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign


ecuacionesNoLineales :: IO Table
ecuacionesNoLineales = do table <- tableNew 9 1 False
                          tableSetColSpacings table 10
                          content <- vBoxNew False 0
                          tableAttachDefaults table content 0 1 0 1
                          
                          labelent <- labelNew (Just "Ingrese la función")
                          entrada <- entryNew
                          boxPackStart content labelent PackNatural 0
                          boxPackStart content  entrada PackNatural 0
                          labelsal <- labelNew (Just "Resultado:")
                          salida  <- entryNew
                          boxPackStart content labelsal PackNatural 0
                          boxPackStart content salida PackNatural 0
                          eval <- buttonNewWithLabel "Evaluar"
                          boxPackStart content eval  PackNatural 0
                          graficar <- buttonNewWithLabel "Graficar"
                          boxPackStart content graficar  PackNatural 0
                          ayuda <- buttonNewWithLabel "Necesitas ayuda?"
                          boxPackStart content ayuda  PackNatural 0
                          
                          {-BUSQUEDAS INCREMENTALES-}

                          busqdInc <- hBoxNew False 0
                          option <- vBoxNew False 0
                          ba <- vBoxNew False 0
                          bb <- vBoxNew False 0 
                          bi <- vBoxNew False 0
                          tableAttachDefaults table busqdInc 0 1 2 3
                          radio2 <- radioButtonNewWithLabel "Busqueda incremental "
                          boxPackStart option radio2 PackNatural 0
                          boxPackStart busqdInc option PackNatural 5
                          boxPackStart busqdInc ba PackNatural 10
                          boxPackStart busqdInc bb PackNatural 10
                          boxPackStart busqdInc bi PackNatural 10
  
   
                          labela <- labelNew (Just " valor inicial  ")
                          miscSetAlignment labela 0 0
                          boxPackStart ba labela PackNatural 0
                          a <- entryNew
                          boxPackStart ba a PackNatural 0

                          labelb <- labelNew (Just "incremento ")
                          miscSetAlignment labelb 0 0
                          boxPackStart bb labelb PackNatural 0
                          b <- entryNew 
                          boxPackStart bb b PackNatural 0
  
                          labeli <- labelNew (Just " iteraciones ")
                          miscSetAlignment labeli 0 0
                          boxPackStart bi labeli PackNatural 0
                          i <- entryNew
                          boxPackStart bi i PackNatural 0
   
                          {-BISECCION-}
  
                          biseccion <- hBoxNew False 0 
                          optionb <- vBoxNew False 5
                          bab <- vBoxNew False 0
                          bbb <- vBoxNew False 0 
                          btolb<- vBoxNew False 0
                          bib <- vBoxNew False 0
                           
                          tableAttachDefaults table biseccion 0 1 3 4
                          radio3 <- radioButtonNewWithLabelFromWidget radio2 "Biseccion "
                          boxPackStart biseccion optionb PackNatural 5
                          boxPackStart optionb radio3 PackNatural 10
                          boxPackStart biseccion bab PackNatural 10
                          boxPackStart biseccion bbb PackNatural 10
                          boxPackStart biseccion btolb PackNatural 10
                          boxPackStart biseccion bib PackNatural 10
  
                          labelab     <- labelNew (Just "valor de a ")
                          miscSetAlignment labelab 0 0
                          boxPackStart bab labelab PackNatural 0
                          ab <- entryNew
                          boxPackStart bab ab PackNatural 0
  
                          labelbb <- labelNew (Just "valor de b ")
                          miscSetAlignment labelbb 0 0
                          boxPackStart bbb labelbb PackNatural 0
                          bb <- entryNew 
                          boxPackStart bbb bb PackNatural 0
   
                          labeltolb     <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltolb 0 0
                          boxPackStart btolb labeltolb PackNatural 0
                          tol <- entryNew
                          boxPackStart btolb tol PackNatural 0

                          labelib     <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelib 0 0
                          boxPackStart bib labelib PackNatural 0
                          ib <- entryNew
                          boxPackStart bib ib PackNatural 0
                          return table
                          {-REGLAFALSA-}
  
                          reglaFalsa <- hBoxNew False 6
                          optionr <- vBoxNew False 0
                          bar <- vBoxNew False 0
                          bbr <- vBoxNew False 0 
                          btolr<- vBoxNew False 0
                          bir <- vBoxNew False 0
 
                          tableAttachDefaults table reglaFalsa 0 1 4 5
                          radio4 <- radioButtonNewWithLabelFromWidget radio3 "Regla falsa "
                          boxPackStart reglaFalsa optionr PackNatural 5
                          boxPackStart optionr radio4 PackNatural 10
                          boxPackStart reglaFalsa bar PackNatural 10
                          boxPackStart reglaFalsa bbr PackNatural 10
                          boxPackStart reglaFalsa btolr PackNatural 10
                          boxPackStart reglaFalsa bir PackNatural 10

  
                          labelar <- labelNew (Just "valor de a ")
                          miscSetAlignment labelar 0 0
                          boxPackStart bar labelar PackNatural 0
                          ar <- entryNew
                          boxPackStart bar ar PackNatural 0
  
                          labelbr <- labelNew (Just "valor de b ")
                          miscSetAlignment labelbr 0 0
                          boxPackStart bbr labelbr PackNatural 0
                          br <- entryNew 
                          boxPackStart bbr br PackNatural 0
   
                          labeltolr     <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltolr 0 0
                          boxPackStart btolr labeltolr PackNatural 0
                          tolr <- entryNew
                          boxPackStart btolr tolr PackNatural 0

                          labelir <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelir 0 0
                          boxPackStart bir labelir PackNatural 0
                          ir <- entryNew
                          boxPackStart bir ir PackNatural 0
                          
                          {-PUNTO FIJO-}
                          puntoFijo <- hBoxNew False 6 
                          optionp <- vBoxNew False 0
                          bap <- vBoxNew False 0
                          bbp <- vBoxNew False 0 
                          btolp<- vBoxNew False 0
                          bip <- vBoxNew False 0
 
                          tableAttachDefaults table puntoFijo 0 1 5 6
                          radio5 <- radioButtonNewWithLabelFromWidget radio4 "Punto Fijo "
                          boxPackStart puntoFijo optionp PackNatural 5
                          boxPackStart optionp radio5 PackNatural 10
                          boxPackStart puntoFijo bap PackNatural 10
                          boxPackStart puntoFijo bbp PackNatural 10
                          boxPackStart puntoFijo btolp PackNatural 10
                          boxPackStart puntoFijo bip PackNatural 10
  
                          labelap <- labelNew (Just "valor inicial ")
                          miscSetAlignment labelap 0 0
                          boxPackStart bap labelap PackNatural 0
                          ap <- entryNew
                          boxPackStart bap ap PackNatural 0
  
                          labelbp <- labelNew (Just "función g ")
                          miscSetAlignment labelbp 0 0
                          boxPackStart bbp labelbp PackNatural 0
                          bp <- entryNew 
                          boxPackStart bbp bp PackNatural 0
   
                          labeltolp     <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltolp 0 0
                          boxPackStart btolp labeltolp PackNatural 0
                          tolp <- entryNew
                          boxPackStart btolp tolp PackNatural 0

                          labelip <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelip 0 0
                          boxPackStart bip labelip PackNatural 0
                          ip <- entryNew
                          boxPackStart bip ip PackNatural 0
                          
                          {-NEWTON-}
                          
                          newton <- hBoxNew False 0
                          optionn <- vBoxNew False 0
                          ban <- vBoxNew False 0
                          bbn <- vBoxNew False 0 
                          btoln<- vBoxNew False 0
                          bin <- vBoxNew False 0
 
                          tableAttachDefaults table newton 0 1 6 7 
                          radio6 <- radioButtonNewWithLabelFromWidget radio5 "Newton "
                          boxPackStart newton optionn PackNatural 5
                          boxPackStart optionn radio6 PackNatural 10
                          boxPackStart newton ban PackNatural 10
                          boxPackStart newton bbn PackNatural 10
                          boxPackStart newton btoln PackNatural 10
                          boxPackStart newton bin PackNatural 10
  
                          labelan <- labelNew (Just "valor inicial ")
                          miscSetAlignment labelan 0 0
                          boxPackStart ban labelan PackNatural 0
                          an <- entryNew
                          boxPackStart ban an PackNatural 0
  
                          labelbn <- labelNew (Just "derivada ")
                          miscSetAlignment labelbn 0 0
                          boxPackStart bbn labelbn PackNatural 0
                          bn <- entryNew 
                          boxPackStart bbn bn PackNatural 0
   
                          labeltoln   <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltoln 0 0
                          boxPackStart btoln labeltoln PackNatural 0
                          toln <- entryNew
                          boxPackStart btoln toln PackNatural 0

                          labelin <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelin 0 0
                          boxPackStart bin labelin PackNatural 0
                          inn <- entryNew
                          boxPackStart bin inn PackNatural 0
                          
                          {-SECANTE-} 
                          secante <- hBoxNew False 10 
                          options <- vBoxNew False 0
                          bas <- vBoxNew False 0
                          bbs <- vBoxNew False 0 
                          btols<- vBoxNew False 0
                          bis <- vBoxNew False 0
 
                          tableAttachDefaults table secante 0 1 7 8 
                          radio7 <- radioButtonNewWithLabelFromWidget radio6 "Secante "
                          boxPackStart secante options PackNatural 5
                          boxPackStart options radio7 PackNatural 10
                          boxPackStart secante bas PackNatural 10
                          boxPackStart secante bbs PackNatural 10
                          boxPackStart secante btols PackNatural 10
                          boxPackStart secante bis PackNatural 10
  
                          labelas <- labelNew (Just "valor inicial ")
                          miscSetAlignment labelas 0 0
                          boxPackStart bas labelas PackNatural 0
                          as <- entryNew
                          boxPackStart bas as PackNatural 0
  
                          labelbs <- labelNew (Just "segundo valor ")
                          miscSetAlignment labelbs 0 0
                          boxPackStart bbs labelbs PackNatural 0
                          bs <- entryNew 
                          boxPackStart bbs bs PackNatural 0
   
                          labeltols   <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltols 0 0
                          boxPackStart btols labeltols PackNatural 0
                          tols <- entryNew
                          boxPackStart btols tols PackNatural 0

                          labelis <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelis 0 0
                          boxPackStart bis labelis PackNatural 0
                          ins <- entryNew
                          boxPackStart bis ins PackNatural 0
                          
                          {-RAICES MULTIPLES-}
                          raices <- hBoxNew False 0 
                          optionrm <- vBoxNew False 0
                          barm <- vBoxNew False 0
                          btolrm<- vBoxNew False 0
                          birm <- vBoxNew False 0
 
                          tableAttachDefaults table raices 0 1 8 9 
                          radio8 <- radioButtonNewWithLabelFromWidget radio7 "Raices Multiples "
                          boxPackStart raices optionrm PackNatural 0
                          boxPackStart optionrm radio8 PackNatural 5
                          boxPackStart raices barm PackNatural 10
                          boxPackStart raices btolrm PackNatural 10
                          boxPackStart raices birm PackNatural 10
  
                          labelarm <- labelNew (Just "valor inicial ")
                          miscSetAlignment labelarm 0 0
                          boxPackStart barm labelarm PackNatural 0
                          arm <- entryNew
                          boxPackStart barm arm PackNatural 0
  
   
                          labeltolrm   <- labelNew (Just "tolerancia ")
                          miscSetAlignment labeltolrm 0 0
                          boxPackStart btolrm labeltolrm PackNatural 0
                          tolrm <- entryNew
                          boxPackStart btolrm tolrm PackNatural 0

                          labelirm <- labelNew (Just "iteraciones ")
                          miscSetAlignment labelirm 0 0
                          boxPackStart birm labelirm PackNatural 0
                          inrm <- entryNew
                          boxPackStart birm inrm PackNatural 0
                          

                          {-FUNCIONES PARA CONTROLAR LOS EVENTOS-}
                          toggleButtonSetActive radio2 True
                          onToggled radio2 (setRadioState radio2)
                          onToggled radio3 (setRadioState radio3)
                          onToggled radio4 (setRadioState radio4)
                          onToggled radio5 (setRadioState radio5)

                          {-CAPTURA DE EVENTO DEL GRAFICADOR-}
                          onClicked ayuda $ do 
                            ayudaENL
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
                                        process f ap bp tolp ip "Punto Fijo " salida
                                 else        
                                     if (unsafePerformIO(toggleButtonGetActive radio4))
                                      then do
                                             ar <- get ar entryText
                                             br <- get br entryText
                                             tolr <- get tolr entryText
                                             ir <- get ir entryText
                                             process f ar br tolr ir "Regla falsa " salida
                                      else
                                          if (unsafePerformIO(toggleButtonGetActive radio3))
                                           then do
                                                  ab <- get ab entryText
                                                  bb <- get bb entryText
                                                  tol <- get tol entryText
                                                  ib <- get ib entryText
                                                  process f ab bb tol ib "Biseccion "salida
                                            else
                                                if (unsafePerformIO(toggleButtonGetActive radio6))
                                                 then do
                                                        an <- get an entryText
                                                        bn <- get bn entryText
                                                        toln <- get toln entryText
                                                        inn <- get inn entryText
                                                        process f an bn toln inn "Newton " salida
                                                 else 
                                                     if (unsafePerformIO(toggleButtonGetActive radio7))
                                                      then do
                                                             as <- get as entryText
                                                             bs <- get bs entryText
                                                             tols <- get tols entryText
                                                             ins <- get ins entryText
                                                             process f as bs tols ins "Secante " salida
                                                      else 
                                                          if (unsafePerformIO(toggleButtonGetActive radio8))
                                                           then do
                                                                  arm <- get arm entryText
                                                                  tolrm <- get tolrm entryText
                                                                  irm <- get inrm entryText
                                                                  process f arm arm tolrm irm "raices " salida
                                                           else do
                                                                  a <- get a entryText
                                                                  b <- get b entryText
                                                                  i <- get i entryText
                                                                  process f a b i i "Busqueda incremental " salida
 
  
                          return table                                       
                                   
                              