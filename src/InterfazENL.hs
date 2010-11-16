module InterfazENL where
import EcuacionesNoLineales
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
                          boxPackStart content labelent PackNatural 5
                          boxPackStart content  entrada PackNatural 5
                          labelsal <- labelNew (Just "Resultado")
                          salida  <- entryNew
                          boxPackStart content labelsal PackNatural 5
                          boxPackStart content salida PackNatural 5
                          eval <- buttonNewWithLabel "Evaluar"
                          boxPackStart content eval  PackNatural 0
                          graficar <- buttonNewWithLabel "Graficar"
                          boxPackStart content graficar  PackNatural 0
                          ayuda <- buttonNewWithLabel "¿Necesitas ayuda?"
                          boxPackStart content ayuda  PackNatural 0
                          
                          {-TIPO DE ERROR-}
                          errores <- hBoxNew False 0
                          radio1 <- radioButtonNewWithLabel "Error Absoluto"
                          containerAdd errores radio1
                          radioe <- radioButtonNewWithLabelFromWidget radio1 "Error Relativo"
                          containerAdd errores radioe
                          containerAdd content errores

                          {-BUSQUEDAS INCREMENTALES-}
                          
                          busqdInc <- hBoxNew False 0
                          option <- vBoxNew False 0
                          ba <- vBoxNew False 0
                          bb <- vBoxNew False 0 
                          bi <- vBoxNew False 0
                          tableAttachDefaults table busqdInc 0 1 2 3
                          radio2 <- radioButtonNewWithLabel "Busquedas incrementales"
                          boxPackStart option radio2 PackNatural 10
                          boxPackStart busqdInc option PackNatural 10
                          boxPackStart busqdInc ba PackNatural 10
                          boxPackStart busqdInc bb PackNatural 10
                          boxPackStart busqdInc bi PackNatural 10
  
   
                          labela <- labelNew (Just "Valor inicial")
                          miscSetAlignment labela 0 0
                          boxPackStart ba labela PackNatural 0
                          a <- entryNew
                          boxPackStart ba a PackNatural 0

                          labelb <- labelNew (Just "Incremento")
                          miscSetAlignment labelb 0 0
                          boxPackStart bb labelb PackNatural 0
                          b <- entryNew 
                          boxPackStart bb b PackNatural 0
  
                          labeli <- labelNew (Just "Iteraciones")
                          miscSetAlignment labeli 0 0
                          boxPackStart bi labeli PackNatural 0
                          i <- entryNew
                          boxPackStart bi i PackNatural 0
   
                          {-BISECCION-}
  
                          biseccionb <- hBoxNew False 0 
                          optionb <- vBoxNew False 0
                          bab <- vBoxNew False 0
                          bbb <- vBoxNew False 0 
                          btolb<- vBoxNew False 0
                          bib <- vBoxNew False 0
                           
                          tableAttachDefaults table biseccionb 0 1 3 4
                          radio3 <- radioButtonNewWithLabelFromWidget radio2 "Bisección"
                          boxPackStart biseccionb optionb PackNatural 10
                          boxPackStart optionb radio3 PackNatural 12
                          boxPackStart biseccionb bab PackNatural 12
                          boxPackStart biseccionb bbb PackNatural 12
                          boxPackStart biseccionb btolb PackNatural 12
                          boxPackStart biseccionb bib PackNatural 12
  
                          labelab     <- labelNew (Just "Valor de a")
                          miscSetAlignment labelab 0 0
                          boxPackStart bab labelab PackNatural 0
                          ab <- entryNew
                          boxPackStart bab ab PackNatural 0
  
                          labelbb <- labelNew (Just "Valor de b")
                          miscSetAlignment labelbb 0 0
                          boxPackStart bbb labelbb PackNatural 0
                          bb <- entryNew 
                          boxPackStart bbb bb PackNatural 0
   
                          labeltolb     <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltolb 0 0
                          boxPackStart btolb labeltolb PackNatural 0
                          tol <- entryNew
                          boxPackStart btolb tol PackNatural 0

                          labelib     <- labelNew (Just "Iteraciones")
                          miscSetAlignment labelib 0 0
                          boxPackStart bib labelib PackNatural 0
                          ib <- entryNew
                          boxPackStart bib ib PackNatural 0
                          return table

                          {-REGLAFALSA-}
  
                          reglaFalsab <- hBoxNew False 0
                          optionr <- vBoxNew False 0
                          bar <- vBoxNew False 0
                          bbr <- vBoxNew False 0 
                          btolr<- vBoxNew False 0
                          bir <- vBoxNew False 0
 
                          tableAttachDefaults table reglaFalsab 0 1 4 5
                          radio4 <- radioButtonNewWithLabelFromWidget radio3 "Regla Falsa"
                          boxPackStart reglaFalsab optionr PackNatural 10
                          boxPackStart optionr radio4 PackNatural 12
                          boxPackStart reglaFalsab bar PackNatural 12
                          boxPackStart reglaFalsab bbr PackNatural 12
                          boxPackStart reglaFalsab btolr PackNatural 12
                          boxPackStart reglaFalsab bir PackNatural 12

  
                          labelar <- labelNew (Just "Valor de a")
                          miscSetAlignment labelar 0 0
                          boxPackStart bar labelar PackNatural 0
                          ar <- entryNew
                          boxPackStart bar ar PackNatural 0
  
                          labelbr <- labelNew (Just "Valor de b")
                          miscSetAlignment labelbr 0 0
                          boxPackStart bbr labelbr PackNatural 0
                          br <- entryNew 
                          boxPackStart bbr br PackNatural 0
   
                          labeltolr     <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltolr 0 0
                          boxPackStart btolr labeltolr PackNatural 0
                          tolr <- entryNew
                          boxPackStart btolr tolr PackNatural 0

                          labelir <- labelNew (Just "Iteraciones")
                          miscSetAlignment labelir 0 0
                          boxPackStart bir labelir PackNatural 0
                          ir <- entryNew
                          boxPackStart bir ir PackNatural 0
                          
                          {-PUNTO FIJO-}

                          bpuntoFijo <- hBoxNew False 0 
                          optionp <- vBoxNew False 0
                          bap <- vBoxNew False 0
                          bbp <- vBoxNew False 0 
                          btolp<- vBoxNew False 0
                          bip <- vBoxNew False 0
 
                          tableAttachDefaults table bpuntoFijo 0 1 5 6
                          radio5 <- radioButtonNewWithLabelFromWidget radio4 "Punto Fijo"
                          boxPackStart bpuntoFijo optionp PackNatural 10
                          boxPackStart optionp radio5 PackNatural 12
                          boxPackStart bpuntoFijo bap PackNatural 12
                          boxPackStart bpuntoFijo bbp PackNatural 12
                          boxPackStart bpuntoFijo btolp PackNatural 12
                          boxPackStart bpuntoFijo bip PackNatural 12
  
                          labelap <- labelNew (Just "Valor inicial")
                          miscSetAlignment labelap 0 0
                          boxPackStart bap labelap PackNatural 0
                          ap <- entryNew
                          boxPackStart bap ap PackNatural 0
  
                          labelbp <- labelNew (Just "Función g")
                          miscSetAlignment labelbp 0 0
                          boxPackStart bbp labelbp PackNatural 0
                          bp <- entryNew 
                          boxPackStart bbp bp PackNatural 0
   
                          labeltolp     <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltolp 0 0
                          boxPackStart btolp labeltolp PackNatural 0
                          tolp <- entryNew
                          boxPackStart btolp tolp PackNatural 0

                          labelip <- labelNew (Just "Iteraciones")
                          miscSetAlignment labelip 0 0
                          boxPackStart bip labelip PackNatural 0
                          ip <- entryNew
                          boxPackStart bip ip PackNatural 0
                          
                          {-NEWTON-}
                          
                          newtonb <- hBoxNew False 10
                          optionn <- vBoxNew False 10
                          ban <- vBoxNew False 0
                          bbn <- vBoxNew False 0 
                          btoln<- vBoxNew False 0
                          bin <- vBoxNew False 0
 
                          tableAttachDefaults table newtonb 0 1 6 7 
                          radio6 <- radioButtonNewWithLabelFromWidget radio5 "Newton"
                          boxPackStart newtonb optionn PackNatural 10
                          boxPackStart optionn radio6 PackNatural 12
                          boxPackStart newtonb ban PackNatural 12
                          boxPackStart newtonb bbn PackNatural 12
                          boxPackStart newtonb btoln PackNatural 12
                          boxPackStart newtonb bin PackNatural 12
  
                          labelan <- labelNew (Just "Valor inicial")
                          miscSetAlignment labelan 0 0
                          boxPackStart ban labelan PackNatural 0
                          an <- entryNew
                          boxPackStart ban an PackNatural 0
  
                          labelbn <- labelNew (Just "Derivada")
                          miscSetAlignment labelbn 0 0
                          boxPackStart bbn labelbn PackNatural 0
                          bn <- entryNew 
                          boxPackStart bbn bn PackNatural 0
   
                          labeltoln   <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltoln 0 0
                          boxPackStart btoln labeltoln PackNatural 0
                          toln <- entryNew
                          boxPackStart btoln toln PackNatural 0

                          labelin <- labelNew (Just "Iteraciones")
                          miscSetAlignment labelin 0 0
                          boxPackStart bin labelin PackNatural 0
                          inn <- entryNew
                          boxPackStart bin inn PackNatural 0
                          
                          {-SECANTE-} 

                          secantep <- hBoxNew False 10 
                          options <- vBoxNew False 10
                          bas <- vBoxNew False 0
                          bbs <- vBoxNew False 0 
                          btols<- vBoxNew False 0
                          bis <- vBoxNew False 0
 
                          tableAttachDefaults table secantep 0 1 7 8 
                          radio7 <- radioButtonNewWithLabelFromWidget radio6 "Secante"
                          boxPackStart secantep options PackNatural 10
                          boxPackStart options radio7 PackNatural 12
                          boxPackStart secantep bas PackNatural 12
                          boxPackStart secantep bbs PackNatural 12
                          boxPackStart secantep btols PackNatural 12
                          boxPackStart secantep bis PackNatural 12
  
                          labelas <- labelNew (Just "Primer valor inicial")
                          miscSetAlignment labelas 0 0
                          boxPackStart bas labelas PackNatural 0
                          as <- entryNew
                          boxPackStart bas as PackNatural 0
  
                          labelbs <- labelNew (Just "Segundo valor inicial")
                          miscSetAlignment labelbs 0 0
                          boxPackStart bbs labelbs PackNatural 0
                          bs <- entryNew 
                          boxPackStart bbs bs PackNatural 0
   
                          labeltols   <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltols 0 0
                          boxPackStart btols labeltols PackNatural 0
                          tols <- entryNew
                          boxPackStart btols tols PackNatural 0

                          labelis <- labelNew (Just "Iteraciones")
                          miscSetAlignment labelis 0 0
                          boxPackStart bis labelis PackNatural 0
                          ins <- entryNew
                          boxPackStart bis ins PackNatural 0
                          
                          {-RAICES MULTIPLES-}

                          raices <- hBoxNew False 10
                          optionrm <- vBoxNew False 10
                          barm <- vBoxNew False 0
                          btolrm<- vBoxNew False 0
                          birm <- vBoxNew False 0
 
                          tableAttachDefaults table raices 0 1 8 9 
                          radio8 <- radioButtonNewWithLabelFromWidget radio7 "Raíces Múltiples"
                          boxPackStart raices optionrm PackNatural 10
                          boxPackStart optionrm radio8 PackNatural 5
                          boxPackStart raices barm PackNatural 12
                          boxPackStart raices btolrm PackNatural 12
                          boxPackStart raices birm PackNatural 12
  
                          labelarm <- labelNew (Just "Valor inicial")
                          miscSetAlignment labelarm 0 0
                          boxPackStart barm labelarm PackNatural 0
                          arm <- entryNew
                          boxPackStart barm arm PackNatural 0
 
   
                          labeltolrm   <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltolrm 0 0
                          boxPackStart btolrm labeltolrm PackNatural 0
                          tolrm <- entryNew
                          boxPackStart btolrm tolrm PackNatural 0

                          labelirm <- labelNew (Just "Iteraciones")
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
                                fun <- parseIO pFunc (funScanTxt f)
                                if (unsafePerformIO(toggleButtonGetActive radio5))
                                 then do
                                        sap <- get ap entryText
                                        sbp <- get bp entryText
                                        stolp <- get tolp entryText
                                        app <- parseIO pFunc (funScanTxt sap)
                                        bpp <- parseIO pFunc (funScanTxt sbp)
                                        tolpp <- parseIO pFunc (funScanTxt stolp)
                                        ipp <- get ip entryText
                                        let r = puntoFijo fun bpp app tolpp (read ipp) "abs"
                                        set salida [entryText := show(r)]
                                 else        
                                     if (unsafePerformIO(toggleButtonGetActive radio4))
                                      then do
                                             sar <- get ar entryText
                                             sbr <- get br entryText
                                             stolr <- get tolr entryText
                                             ar <- parseIO pFunc (funScanTxt sar)
                                             br <- parseIO pFunc (funScanTxt sbr)
                                             tolr <- parseIO pFunc (funScanTxt stolr)
                                             ir <- get ir entryText
                                             let r = reglaFalsa fun ar br tolr (read ir) "abs"
                                             set salida [entryText := show(r)]
                                      else
                                          if (unsafePerformIO(toggleButtonGetActive radio3))
                                           then do
                                                  sab <- get ab entryText
                                                  sbb <- get bb entryText
                                                  stolb <- get tol entryText
                                                  ab <- parseIO pFunc (funScanTxt sab)
                                                  bb <- parseIO pFunc (funScanTxt sbb)
                                                  tolb <- parseIO pFunc (funScanTxt stolb)
                                                  ib <- get ib entryText
                                                  let r = biseccion fun ab bb tolb (read ib) "abs"
                                                  set salida [entryText := (show r)]
                                            else
                                                if (unsafePerformIO(toggleButtonGetActive radio6))
                                                 then do
                                                        san <- get an entryText
                                                        sbn <- get bn entryText
                                                        stoln <- get toln entryText
                                                        inn <- get inn entryText
                                                        an <- parseIO pFunc (funScanTxt san)
                                                        bn <- parseIO pFunc (funScanTxt sbn)
                                                        toln <- parseIO pFunc (funScanTxt stoln)
                                                        let r = newton fun bn an toln (read inn) "abs"
                                                        set salida [entryText := r]
                                                 else 
                                                     if (unsafePerformIO(toggleButtonGetActive radio7))
                                                      then do
                                                             sas <- get as entryText
                                                             sbs <- get bs entryText
                                                             stols <- get tols entryText
                                                             ins <- get ins entryText
                                                             as <- parseIO pFunc (funScanTxt sas)
                                                             bs <- parseIO pFunc (funScanTxt sbs)
                                                             tols <- parseIO pFunc (funScanTxt stols)
                                                             let r = secante fun as bs tols (read ins) "abs"
                                                             set salida [entryText := r]
                                                      else 
                                                          if (unsafePerformIO(toggleButtonGetActive radio8))
                                                           then do
                                                                  sarm <- get arm entryText
                                                                  stolrm <- get tolrm entryText
                                                                  irm <- get inrm entryText
                                                                  arm <- parseIO pFunc (funScanTxt sarm)
                                                                  tolrm <- parseIO pFunc (funScanTxt stolrm)
                                                                  let r = raicesMult fun arm tolrm (read irm) "abs"
                                                                  set salida [entryText := r ]
                                                           else do
                                                                  sa <- get a entryText
                                                                  sb <- get b entryText
                                                                  i <- get i entryText
                                                                  a <- parseIO pFunc (funScanTxt sa)
                                                                  b <- parseIO pFunc (funScanTxt sb) 
                                                                  let r = busqdIncremental fun a b (read i) 
                                                                  set salida [entryText := (show r)]
  
                          return table                                       
                                   
                              