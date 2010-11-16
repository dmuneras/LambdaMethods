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
                      table <- tableNew 4 1 False
                      content <- vBoxNew False 4
                      sistem <- vBoxNew False 4
                      grafMatriz <- textViewNew
                      tableAttachDefaults table content 0 1 0 1
                      values <- entryNew
                      valuesb <- entryNew
                      labelv <- labelNew (Just "Ingrese matriz aumentada para Eliminaciones Gaussianas o matriz de coeficientes para Métodos Iterativos (Por filas y elementos separados por espacios)")
                      labelb <- labelNew (Just "Ingrese el vector de términos independientes (Elementos separados por espacios)")
                      labels <- labelNew (Just "Valores de las incognitas")
                      eval <- buttonNewWithLabel "Evaluar"
                      boxPackStart content sistem PackNatural 0
                      boxPackStart sistem labelv PackNatural 0
                      boxPackStart sistem values PackNatural 0
                      boxPackStart sistem labelb PackNatural 0
                      boxPackStart sistem valuesb PackNatural 0

                      {-MATRIZ RESULTANTE-}
                      tableAttachDefaults table grafMatriz 0 1 1 2
                      textViewSetEditable grafMatriz False
                      buffer <- textViewGetBuffer grafMatriz
                      labelmm <- labelNew (Just "Matriz resultante")
                      labelm <- labelNew (Just "Resultados")
                      containerAdd sistem labelm
                      containerAdd sistem labelmm
                      
                      {-RESULTADOS-}
                      resu <- vBoxNew False 0
                      salida <- entryNew
                      tableAttachDefaults table resu 0 1 2 3
                      boxPackStart resu labels PackNatural 0
                      boxPackStart resu salida PackNatural 0
                      boxPackStart resu eval PackNatural 0
                      
                      
                      {-METODOS DIRECTOS-}
                      directosp <- vBoxNew False 5
                      directosaux <- hBoxNew False 5
                      directos <- vBoxNew False 5
                      directosh <- vBoxNew False 5
                      directoshh <- vBoxNew False 5
                      directoshhh <- vBoxNew False 5
                      directoshhhh <- vBoxNew False 5
                      directoshhhhh <- vBoxNew False 5
                      tama <- vBoxNew False 0
                      tableAttachDefaults table directosp 0 1 3 4
                      tamaño <- entryNew
                      labeld <- labelNew (Just "Métodos directos")
                      labeltama <- labelNew (Just "Tamaño del sistema")
                      containerAdd tama labeltama
                      containerAdd tama tamaño
                      containerAdd directosp labeld
                      containerAdd directosaux directos
                      containerAdd directosaux tama
                      containerAdd directosaux directosh
                      containerAdd directosaux directoshh
                      containerAdd directosaux directoshhh
                      containerAdd directosaux directoshhhh
                      containerAdd directosaux directoshhhhh
                      containerAdd directosp directosaux

                      {-ELIMINACION GAUSSIANA SIMPLE-}
                      gsim <- vBoxNew False 5
                      option <- hBoxNew False 0
                      containerAdd directos gsim
                      radio1 <- radioButtonNewWithLabel "Eliminación Gaussiana Simple"
                      boxPackStart option radio1 PackNatural 5
                      boxPackStart gsim option PackNatural 0
 
                      {-ELIMINACION CON PIVOTEO PARCIAL-}
                      gparcial <- hBoxNew False 5
                      optionp <- vBoxNew False 0
                      containerAdd directos gparcial
                      radio2 <- radioButtonNewWithLabelFromWidget radio1 "Eliminación con Pivoteo Parcial"
                      boxPackStart optionp radio2 PackNatural 5
                      boxPackStart gparcial optionp PackNatural 0
                      
                      {-ELIMINACION CON PIVOTEO TOTAL-}
                      gtotal <- hBoxNew False 5
                      optiont <- vBoxNew False 0
                      containerAdd directos gtotal
                      radio3 <- radioButtonNewWithLabelFromWidget radio2 "Eliminación con Pivoteo Total"
                      boxPackStart optiont radio3 PackNatural 5
                      boxPackStart gtotal optiont PackNatural 0
                 
                    
                      {-METODOS ITERATIVOS-}
                     
                      viterativo <- hBoxNew False 5
                      iterativo <- vBoxNew False 5
                      labeliter <- labelNew (Just "Métodos iterativos")
                      labelen <- labelNew (Just "Ingrese los respectivos valores y luego seleccion el método a usar")
                      containerAdd iterativo labeliter
                      tableAttachDefaults table iterativo 0 1 4 5
                    
                      bxoj <- vBoxNew False 5
                      bnj <- vBoxNew False 5
                      blaj <- vBoxNew False 5
                      btolj <- vBoxNew False 5
                      bij <- vBoxNew False 5
  
                      
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
                      optioniter <- vBoxNew False 5
                      jaco <- hBoxNew False 5
                      optionj <- vBoxNew False 5
                      radio4 <- radioButtonNewWithLabelFromWidget radio3 "Jacobi"
                      boxPackStart optionj radio4 PackNatural 5
                      boxPackStart jaco optionj PackNatural 0
                      
                      
                      seidel <- hBoxNew False 5
                      options <- vBoxNew False 5
                      radio5 <- radioButtonNewWithLabelFromWidget radio4 "Gauss Seidel"
                      boxPackStart options radio5 PackNatural 5
                      boxPackStart seidel options PackNatural 0
                      

                      containerAdd optioniter jaco
                      containerAdd optioniter seidel
                      containerAdd viterativo optioniter
                      containerAdd iterativo viterativo
                      boxPackStart viterativo bxoj PackNatural 0
                      boxPackStart viterativo bnj PackNatural 0
                      boxPackStart viterativo blaj PackNatural 0
                      boxPackStart viterativo btolj PackNatural 0
                      boxPackStart viterativo bij PackNatural 0
                     
                      onClicked eval $ do
                        s <- get values entryText
                        n <- get tamaño entryText
                        au <- parseIO pListDouble (funScanTxt(s))
                        if (unsafePerformIO(toggleButtonGetActive radio1))
                         then do
                               let result = eGaussSim (leerMatrizAu (read n) au) (read n)
                               set salida [entryText := show(result)]
                               let mat = mtos'(matrizEGaussSim (leerMatrizAu (read n) au) (read n)) (read n)
                               textBufferSetText buffer (show mat)
                         else
                            if (unsafePerformIO(toggleButtonGetActive radio2))
                             then do
                                   let result = eGaussPParcial (leerMatrizAu (read n) au) (read n)
                                   set salida [entryText := show(result)]
                                   let mat = mtos'(matrizEGaussParcial (leerMatrizAu (read n) au) (read n)) (read n)
                                   textBufferSetText buffer (show mat)
                             else
                                if (unsafePerformIO(toggleButtonGetActive radio3))
                                 then do
                                       let result = eGaussPTotal (leerMatrizAu (read n) au) (read n)
                                       set salida [entryText := show(result)]
                                       let mat = mtos'(matrizEGaussTotal(leerMatrizAu (read n) au) (read n)) (read n)
                                       textBufferSetText buffer (show mat)
                                       
                                 else do
                                        sxo <- get xoj entryText
                                        sb <- get valuesb entryText
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
                                                let result = jacobi (leerMatriz (read ni) au) (leerXo (read ni) xo) (leerB (read ni) b) (read ni) la tol (read i)
                                                set salida [entryText := (show result)]
                                         else do
                                                let result = gaussSeidel (leerMatriz (read ni) au) (leerXo (read ni) xo) (leerB (read ni) b) (read ni) la tol (read i)
                                                set salida [entryText := (show result)]
                                        
                      return table
                      
                      
    

