module InterfazIntegracion where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign
import Integracion


interfaz_integracion :: IO Table
interfaz_integracion = do table <- tableNew 9 1 False
                          tableSetColSpacings table 10
                          content <- vBoxNew False 0
                          tableAttachDefaults table content 0 1 0 1
                          
                          labelent <- labelNew (Just "Ingrese la función")
                          entrada <- entryNew
                          boxPackStart content labelent PackNatural 0
                          boxPackStart content  entrada PackNatural 0
                          labelsal <- labelNew (Just "Resultado")
                          salida  <- entryNew
                          boxPackStart content labelsal PackNatural 0
                          boxPackStart content salida PackNatural 0
                          eval <- buttonNewWithLabel "Evaluar"
                          boxPackStart content eval  PackNatural 0
                          graficar <- buttonNewWithLabel "Graficar"
                          boxPackStart content graficar  PackNatural 0
                          ayuda <- buttonNewWithLabel "¿Necesitas ayuda?"
                          boxPackStart content ayuda  PackNatural 0
                          
                          {-TRAPECIO SENCILLO-}

                          trapecioSenc <- hBoxNew False 40
                          option <- vBoxNew False 0
                          tsa <- vBoxNew False 0
                          tsb <- vBoxNew False 0 
                          tableAttachDefaults table trapecioSenc 0 1 2 3
                          radio2 <- radioButtonNewWithLabel "Trapecio Sencillo "
                          boxPackStart option radio2 PackNatural 5
                          boxPackStart trapecioSenc option PackNatural 0
                          boxPackStart trapecioSenc tsa PackNatural 0
                          boxPackStart trapecioSenc tsb PackNatural 0
           
  
   
                          labela <- labelNew (Just "Limite Inferior")
                          miscSetAlignment labela 0 0
                          boxPackStart tsa labela PackNatural 0
                          ta <- entryNew
                          boxPackStart tsa ta PackNatural 0

                          labelb <- labelNew (Just "Limite Superior")
                          miscSetAlignment labelb 0 0
                          boxPackStart tsb labelb PackNatural 0
                          tb <- entryNew 
                          boxPackStart tsb tb PackNatural 0
   
                          {-TRAPECIO GENERALIZADO-}
  
                          trapecioGe <- hBoxNew False 10
                          optionb <- vBoxNew False 0
                          batg <- vBoxNew False 0
                          bbtg <- vBoxNew False 0 
                          bntg <- vBoxNew False 0
                           
                          tableAttachDefaults table  trapecioGe 0 1 3 4
                          radio3 <- radioButtonNewWithLabelFromWidget radio2 "Trapecio Generalizado "
                          boxPackStart  trapecioGe optionb PackNatural 0
                          boxPackStart  optionb radio3 PackNatural 5
                          boxPackStart  trapecioGe batg PackNatural 0
                          boxPackStart  trapecioGe bbtg PackNatural 0
                          boxPackStart  trapecioGe bntg PackNatural 0
  
                          labelab     <- labelNew (Just "Limite inferior ")
                          miscSetAlignment labelab 0 0
                          boxPackStart batg labelab PackNatural 0
                          atg <- entryNew
                          boxPackStart batg atg PackNatural 0
  
                          labelbb <- labelNew (Just "Limite superior ")
                          miscSetAlignment labelbb 0 0
                          boxPackStart bbtg labelbb PackNatural 0
                          btg <- entryNew 
                          boxPackStart bbtg btg PackNatural 0
                          
                          labelbnb <- labelNew (Just "Número de particiones ")
                          miscSetAlignment labelbnb 0 0
                          boxPackStart bntg labelbnb PackNatural 0
                          ntg <- entryNew 
                          boxPackStart bntg ntg PackNatural 0
   
                         

                          {-SIMPSON 1/3 SENCILLO-}
  
                          simpson13s <- hBoxNew False 10
                          optionr <- vBoxNew False 0
                          bass <- vBoxNew False 0
                          bbss <- vBoxNew False 0 
 
                          tableAttachDefaults table simpson13s 0 1 4 5
                          radio4 <- radioButtonNewWithLabelFromWidget radio3 "Simpson 1/3 Sencillo"
                          boxPackStart  simpson13s optionr PackNatural 0
                          boxPackStart  optionr radio4 PackNatural 5
                          boxPackStart  simpson13s bass PackNatural 0
                          boxPackStart  simpson13s bbss PackNatural 0
                      
  
                          labelar <- labelNew (Just "Limite inferior")
                          miscSetAlignment labelar 0 0
                          boxPackStart bass labelar PackNatural 0
                          ass <- entryNew
                          boxPackStart bass ass PackNatural 0
  
                          labelbr <- labelNew (Just "Limite superior")
                          miscSetAlignment labelbr 0 0
                          boxPackStart bbss labelbr PackNatural 0
                          bss <- entryNew 
                          boxPackStart bbss bss PackNatural 0
                          
                          {-SIMPSON 1/3 GENERALIZADO-}
                          simpson13g <- hBoxNew False 10
                          optionp <- vBoxNew False 0
                          basg <- vBoxNew False 0
                          bbsg <- vBoxNew False 0 
                          bnsg <- vBoxNew False 0
 
                          tableAttachDefaults table simpson13g 0 1 5 6
                          radio5 <- radioButtonNewWithLabelFromWidget radio4 "Simpson 1/3 Generalizado"
                          boxPackStart simpson13g optionp PackNatural 0
                          boxPackStart optionp radio5 PackNatural 5
                          boxPackStart simpson13g basg PackNatural 0
                          boxPackStart simpson13g bbsg PackNatural 0
                          boxPackStart simpson13g bnsg PackNatural 0
  
                          labelap <- labelNew (Just "Limite inferior")
                          miscSetAlignment labelap 0 0
                          boxPackStart basg labelap PackNatural 0
                          asg <- entryNew
                          boxPackStart basg asg PackNatural 0
  
                          labelbp <- labelNew (Just "Limite superior")
                          miscSetAlignment labelbp 0 0
                          boxPackStart bbsg labelbp PackNatural 0
                          bsg <- entryNew 
                          boxPackStart bbsg bsg PackNatural 0
                          
                          labelnp <- labelNew (Just "Número de particiones")
                          miscSetAlignment labelnp 0 0
                          boxPackStart bnsg labelnp PackNatural 0
                          nsg <- entryNew 
                          boxPackStart bnsg nsg PackNatural 0

                          {-SIMPSON 3/8 SENCILLO-}
                          
                          simpson38s <- hBoxNew False 40
                          optionn <- vBoxNew False 0
                          basos <- vBoxNew False 0
                          bbsos <- vBoxNew False 0 
 
                          tableAttachDefaults table simpson38s 0 1 6 7 
                          radio6 <- radioButtonNewWithLabelFromWidget radio5 "Simpson 3/8 Sencillo"
                          boxPackStart simpson38s optionn PackNatural 0
                          boxPackStart optionn radio6 PackNatural 5
                          boxPackStart simpson38s basos PackNatural 0
                          boxPackStart simpson38s bbsos PackNatural 0
  
                          labelan <- labelNew (Just "Limite inferior")
                          miscSetAlignment labelan 0 0
                          boxPackStart basos labelan PackNatural 0
                          asos <- entryNew
                          boxPackStart basos asos PackNatural 0
  
                          labelbn <- labelNew (Just "Limite superior")
                          miscSetAlignment labelbn 0 0
                          boxPackStart bbsos labelbn PackNatural 0
                          bsos <- entryNew 
                          boxPackStart bbsos bsos PackNatural 0
   
                       
                          {-TRAPECIO ITERATIVO-} 
                          traIter <- hBoxNew False 10
                          options <- vBoxNew False 0
                          bati <- vBoxNew False 0
                          bbti <- vBoxNew False 0 
                          btolti<- vBoxNew False 0
                          bnti <- vBoxNew False 0
                          biti <- vBoxNew False 0
 
                          tableAttachDefaults table traIter 0 1 7 8 
                          radio7 <- radioButtonNewWithLabelFromWidget radio6 "Trapecio Iterativo"
                          boxPackStart traIter options PackNatural 0
                          boxPackStart options radio7 PackNatural 5
                          boxPackStart traIter bati PackNatural 0
                          boxPackStart traIter bbti PackNatural 0
                          boxPackStart traIter btolti PackNatural 0
                          boxPackStart traIter bnti PackNatural 0
                          boxPackStart traIter biti PackNatural 0
  
                          labelas <- labelNew (Just "Limite inferior")
                          miscSetAlignment labelas 0 0
                          boxPackStart bati labelas PackNatural 0
                          ati <- entryNew
                          boxPackStart bati ati PackNatural 0
  
                          labelbs <- labelNew (Just "Limite superior")
                          miscSetAlignment labelbs 0 0
                          boxPackStart bbti labelbs PackNatural 0
                          bti <- entryNew 
                          boxPackStart bbti bti PackNatural 0
   
                          labeltols <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltols 0 0
                          boxPackStart btolti labeltols PackNatural 0
                          tolti <- entryNew 
                          boxPackStart btolti tolti PackNatural 0

                          labelns <- labelNew (Just "Número de particiones")
                          miscSetAlignment labelns 0 0
                          boxPackStart bnti labelns PackNatural 0
                          nti <- entryNew 
                          boxPackStart bnti nti PackNatural 0

                          labelis <- labelNew (Just "Número de iteraciones")
                          miscSetAlignment labelis 0 0
                          boxPackStart biti labelis PackNatural 0
                          iti <- entryNew 
                          boxPackStart biti iti PackNatural 0
                          
                          {-SIMPSON 1/3 ITERATIVO-}
                          simpson13iter <- hBoxNew False 10 
                          optionrm <- vBoxNew False 0
                          basi <- vBoxNew False 0
                          bbsi <- vBoxNew False 0
                          btolsi<- vBoxNew False 0
                          bnsi <- vBoxNew False 0
                          bisi <- vBoxNew False 0
 
                          tableAttachDefaults table simpson13iter 0 1 8 9 
                          radio8 <- radioButtonNewWithLabelFromWidget radio7 "Simpson 1/3 Iterativo"
                          boxPackStart simpson13iter optionrm PackNatural 0
                          boxPackStart optionrm radio8 PackNatural 5
                          boxPackStart simpson13iter basi PackNatural 0
                          boxPackStart simpson13iter bbsi PackNatural 0
                          boxPackStart simpson13iter btolsi PackNatural 0
                          boxPackStart simpson13iter bnsi PackNatural 0
                          boxPackStart simpson13iter bisi PackNatural 0
  
                          labelarm <- labelNew (Just "Limite inferior")
                          miscSetAlignment labelarm 0 0
                          boxPackStart basi labelarm PackNatural 0
                          asi <- entryNew
                          boxPackStart basi asi PackNatural 0

                          labelbrm <- labelNew (Just "Limite superior")
                          miscSetAlignment labelbrm 0 0
                          boxPackStart bbsi labelbrm PackNatural 0
                          bsi <- entryNew
                          boxPackStart bbsi bsi PackNatural 0
  
                          labeltolrm   <- labelNew (Just "Tolerancia")
                          miscSetAlignment labeltolrm 0 0
                          boxPackStart btolsi labeltolrm PackNatural 0
                          tolsi <- entryNew
                          boxPackStart btolsi tolsi PackNatural 0

                          labelnrm <- labelNew (Just "Número de particiones")
                          miscSetAlignment labelnrm 0 0
                          boxPackStart bnsi labelnrm PackNatural 0
                          nsi <- entryNew
                          boxPackStart bnsi nsi PackNatural 0

                          labelirm <- labelNew (Just "Número de iteraciones")
                          miscSetAlignment labelirm 0 0
                          boxPackStart bisi labelirm PackNatural 0
                          insi <- entryNew
                          boxPackStart bisi insi PackNatural 0
                          

                          {-FUNCIONES PARA CONTROLAR LOS EVENTOS-}
                          toggleButtonSetActive radio2 True
                          onToggled radio2 (setRadioState radio2)
                          onToggled radio3 (setRadioState radio3)
                          onToggled radio4 (setRadioState radio4)
                          onToggled radio5 (setRadioState radio5)
                          
                          {-CAPTURA DE EVENTO DEL GRAFICADOR-}
                          onClicked ayuda $ do 
                            ayudaint
                          onClicked graficar $ do
                                s <- get entrada entryText
                                f <- parseIO pFunc (funScanTxt s)
                                graficaXY f
                                                    
                          onClicked eval $ do
                                s <- get entrada entryText
                                f <- parseIO pFunc (funScanTxt s)
                                if (unsafePerformIO(toggleButtonGetActive radio2))
                                 then do
                                        ta <- get ta entryText
                                        tb <- get tb entryText
                                        pta <- parseIO pFunc (funScanTxt ta)
                                        ptb <- parseIO pFunc (funScanTxt tb)
                                        let result = trapecioSen f pta ptb
                                        set salida [entryText := show(result)]
                                 else        
                                     if (unsafePerformIO(toggleButtonGetActive radio3))
                                      then do
                                             a <- get atg entryText
                                             b <- get btg entryText
                                             n <- get ntg entryText
                                             pa <- parseIO pFunc (funScanTxt a)
                                             pb <- parseIO pFunc (funScanTxt b)
                                             pn <- parseIO pDouble (funScanTxt n)
                                             let result = trapecioGen f pa pb pn
                                             set salida [entryText := show(result)]
                                      else
                                          if (unsafePerformIO(toggleButtonGetActive radio4))
                                           then do
                                                  ass <- get ass entryText
                                                  bss <- get bss entryText
                                                  pass <- parseIO pFunc (funScanTxt ass)
                                                  pbss <- parseIO pFunc (funScanTxt bss)
                                                  let result = simpson13Sen f pass pbss
                                                  set salida [entryText := show(result)]
                                            else
                                                if (unsafePerformIO(toggleButtonGetActive radio5))
                                                 then do
                                                        asg <- get asg entryText
                                                        bsg <- get bsg entryText
                                                        nsg <- get nsg entryText
                                                        pasg <- parseIO pFunc (funScanTxt asg)
                                                        pbsg <- parseIO pFunc (funScanTxt bsg)
                                                        pnsg <- parseIO pDouble (funScanTxt nsg)
                                                        let result = simpson13Gen f pasg pbsg pnsg
                                                        set salida [entryText := show(result)]
                                                 else 
                                                     if (unsafePerformIO(toggleButtonGetActive radio6))
                                                      then do
                                                             asos<- get asos entryText
                                                             bsos <- get bsos entryText
                                                             pasos <- parseIO pFunc (funScanTxt asos)
                                                             pbsos <- parseIO pFunc (funScanTxt bsos)
                                                             let result = simpson38Sen f pasos pbsos
                                                             set salida [entryText := show(result)]
                                                      else 
                                                          if (unsafePerformIO(toggleButtonGetActive radio7))
                                                           then do
                                                                  a <- get ati entryText
                                                                  b <- get bti entryText 
                                                                  tol <- get tolti entryText
                                                                  n <- get nti entryText
                                                                  i <- get iti entryText
                                                                  pa <- parseIO pFunc (funScanTxt a)
                                                                  pb <- parseIO pFunc (funScanTxt b)
                                                                  ptol <- parseIO pFunc (funScanTxt tol)
                                                                  pn <- parseIO pDouble (funScanTxt n)
                                                                  let result = trapecioIter f pa pb pn ptol (read i) "abs"
                                                                  set salida [entryText := show(result)]
                                                           else do
                                                                  asi <- get asi entryText
                                                                  bsi <- get bsi entryText 
                                                                  tolsi <- get tolsi entryText
                                                                  nsi <- get nsi entryText
                                                                  i <- get insi entryText
                                                                  pasi <- parseIO pFunc (funScanTxt asi)
                                                                  pbsi <- parseIO pFunc (funScanTxt bsi)
                                                                  ptolsi <- parseIO pFunc (funScanTxt tolsi)
                                                                  pnsi <- parseIO pDouble (funScanTxt nsi)
                                                                  let r = simpson13Iter f pasi pbsi pnsi ptolsi (read i) "abs"
                                                                  set salida [entryText := show(r)]
                                                                 
 
  
                          return table                                       
                                   