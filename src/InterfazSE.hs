module InterfazSE where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import SistemasEcuaciones
import UU.Parsing
import FuncionesAuxiliaresSE
import Graphics.UI.Gtk
import Foreign




sistemasEcuaciones :: IO Table
sistemasEcuaciones= do
                      table <- tableNew 3 1 False
                      content <- vBoxNew False 5
                      sistem <- vBoxNew False 5 
                      grafMatriz <- textViewNew 
                      tableAttachDefaults table grafMatriz 0 1 0 1
                      tableAttachDefaults table content 0 1 1 2
                      values <- entryNew
                      valuesb <- entryNew
                      tamaño <- entryNew
                      salida <- entryNew
                      labelv <- labelNew (Just "Ingrese matriz aumentada para Eliminaciones Gaussianas o matriz de coeficientes para Métodos Iterativos (Por filas y elementos separados por espacios")
                      labelb <- labelNew (Just "Ingrese el vector de términos independientes (Elementos separados por espacios)")
                      labelta <- labelNew (Just "Tamaño del sistema")
                      labels <- labelNew (Just "Resultado")
                      eval <- buttonNewWithLabel "Evaluar"
                      boxPackStart content sistem  PackNatural 0
                      boxPackStart sistem  labelv  PackNatural 0
                      boxPackStart sistem  values  PackNatural 0
                      boxPackStart sistem  labelb  PackNatural 0
                      boxPackStart sistem  valuesb PackNatural 0
                      boxPackStart sistem  labelta PackNatural 0
                      boxPackStart sistem  tamaño  PackNatural 0
                      boxPackStart sistem  labels  PackNatural 0
                      boxPackStart sistem  salida  PackNatural 0
                      boxPackStart content eval  PackNatural 0
                      
                      {-MATRIZ RESULTANTE-}
                      textViewSetEditable grafMatriz False
                      buffer <- textViewGetBuffer grafMatriz
                      {-METODOS DIRECTOS-}

                      directos <- hBoxNew False 5
                      labeld <- labelNew (Just "Métodos directos")
                      containerAdd content labeld

                      {-ELIMINACION GAUSSIANA SIMPLE-}
                      gsim <- hBoxNew False 10
                      option <- vBoxNew False 0
                      containerAdd directos gsim
                      radio1 <- radioButtonNewWithLabel "Eliminación Gaussiana Simple"
                      boxPackStart option radio1 PackNatural 5
                      boxPackStart gsim option PackNatural 0
 
                      {-ELIMINACION CON PIVOTEO PARCIAL-}
                      gparcial <- hBoxNew False 10
                      optionp <- vBoxNew False 0
                      containerAdd directos gparcial
                      radio2 <- radioButtonNewWithLabelFromWidget radio1 "Eliminación con Pivoteo Parcial"
                      boxPackStart optionp radio2 PackNatural 5
                      boxPackStart gparcial optionp PackNatural 0
                      
                      {-ELIMINACION CON PIVOTEO TOTAL-}
                      gtotal <- hBoxNew False 25
                      optiont <- vBoxNew False 0
                      containerAdd directos gtotal
                      radio3 <- radioButtonNewWithLabelFromWidget radio2 "Eliminación con Pivoteo Total"
                      boxPackStart optiont radio3 PackNatural 5
                      boxPackStart gtotal optiont PackNatural 0
                      containerAdd content directos
                    
                      {-METODOS ITERATIVOS-}
                     
                      viterativo <- hBoxNew False 5
                      iterativo <- vBoxNew False 5
                      labeliter <- labelNew (Just "Métodos iterativos")
                      labelen <- labelNew (Just "Ingrese los respectivos valores y luego seleccion el método a usar")
                      containerAdd iterativo labeliter
                      tableAttachDefaults table iterativo 0 1 2 3
                    
                      bxoj <- vBoxNew  False 5
                      bnj <- vBoxNew  False 5
                      blaj <- vBoxNew  False 5
                      btolj <- vBoxNew  False 5
                      bij <- vBoxNew  False 5
                      --boxPackStart viterativo labelen PackNatural 0
                      boxPackStart viterativo bxoj PackNatural 0
                      boxPackStart viterativo bnj PackNatural 0
                      boxPackStart viterativo blaj PackNatural 0
                      boxPackStart viterativo btolj PackNatural 0
                      boxPackStart viterativo bij PackNatural 0
                      
                      labela <- labelNew (Just "Valores iniciales")
                      miscSetAlignment labela 0 0
                      boxPackStart bxoj labela PackNatural 0
                      xoj <- entryNew
                      boxPackStart bxoj xoj PackNatural 0
                      
                      labelnj <- labelNew (Just "Tamaño del sistema")
                      miscSetAlignment labelnj 0 0
                      boxPackStart bnj labelnj PackNatural 0
                      nj <- entryNew
                      boxPackStart bnj nj PackNatural 0

                      labellaj <- labelNew (Just "Relajación")
                      miscSetAlignment labellaj 0 0
                      boxPackStart blaj labellaj PackNatural 0
                      laj <- entryNew
                      boxPackStart blaj laj PackNatural 0

                      labeltolj <- labelNew (Just "Tolerancia")
                      miscSetAlignment labeltolj 0 0
                      boxPackStart btolj labeltolj PackNatural 0
                      tolj <- entryNew
                      boxPackStart btolj tolj PackNatural 0

                      labelij <- labelNew (Just "Numero maximo iteraciones")
                      miscSetAlignment labelij 0 0
                      boxPackStart bij labelij PackNatural 0
                      ij <- entryNew
                      boxPackStart bij ij PackNatural 0
                     
                      {-OPCIONES ITERATIVOS-}
                      optioniter <- hBoxNew False 5
                      jaco <- hBoxNew False 5
                      optionj <- vBoxNew False 5
                      radio4 <- radioButtonNewWithLabelFromWidget radio3 "Jacobi"
                      boxPackStart optionj radio4 PackNatural 5
                      boxPackStart jaco optionj PackNatural 0
                      
                      
                      seidel <- hBoxNew False 5
                      options <- vBoxNew False 5
                      radio5 <- radioButtonNewWithLabelFromWidget radio4 "Gauss Seidel"
                      boxPackStart options radio5 PackNatural 5
                      boxPackStart seidel  options PackNatural 0
                      

                      containerAdd optioniter jaco
                      containerAdd optioniter seidel
                      containerAdd iterativo viterativo
                      containerAdd iterativo optioniter 
                     
                      onClicked eval $ do
                        s <- get values entryText
                        n <- get tamaño entryText
                        au <- parseIO pListDouble (funScanTxt(s))
                        if (unsafePerformIO(toggleButtonGetActive radio1))
                         then do 
                               let result = eGaussSim (leerMatrizAu (read n) au) (read n)
                               set salida [entryText := show(result)]
                               let mat = matrizToString(matrizEGaussSim  (leerMatrizAu (read n) au) (read n)) (read n)
                               textBufferSetText buffer (show mat)
                         else 
                            if (unsafePerformIO(toggleButtonGetActive radio2))
                             then do 
                                   let result = eGaussPParcial (leerMatrizAu (read n) au) (read n)
                                   set salida [entryText := show(result)]
                                   let mat = matrizToString(matrizEGaussParcial (leerMatrizAu (read n) au) (read n)) (read n)
                                   textBufferSetText buffer (show mat)
                             else 
                                if (unsafePerformIO(toggleButtonGetActive radio3))
                                 then do 
                                       let result = eGaussPTotal (leerMatrizAu (read n) au) (read n)
                                       set salida [entryText := show(result)]
                                       let mat = matrizToString(matrizEGaussTotal(leerMatrizAu (read n) au) (read n)) (read n)
                                       textBufferSetText buffer (show mat)
                                 else do
                                        sxo <- get xoj entryText
                                        sb <- get  valuesb entryText
                                        sla <- get laj entryText
                                        stol <- get tolj entryText
                                        i <- get ij entryText
                                        ni <- get nj entryText
                                        xo <- parseIO pListDouble (funScanTxt sxo)
                                        b <- parseIO pListDouble (funScanTxt sb)
                                        la <- parseIO pDouble (funScanTxt sla)
                                        tol <- parseIO pDouble (funScanTxt stol)
                                        if (unsafePerformIO(toggleButtonGetActive radio4))
                                         then do 
                                                let result = jacobi (leerMatriz (read ni) au)  (leerXo (read ni) xo) (leerB (read ni) b) (read ni) la tol (read i) 
                                                set salida [entryText :=  (show result)]
                                         else do
                                                let result = gaussSeidel (leerMatriz (read ni) au)  (leerXo (read ni) xo) (leerB (read ni) b) (read ni) la tol (read i) 
                                                set salida [entryText := (show result)]
                                        
                      return table
                      
                      
    

