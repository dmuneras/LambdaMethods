module InterfazSE where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import SistemasEcuaciones
import UU.Parsing
import Graphics.UI.Gtk
import Foreign



sistemasEcuaciones :: IO Table
sistemasEcuaciones= do
                      table <- tableNew 3 1 False
                      content <- vBoxNew False 5
                      sistem <- vBoxNew False 5 
                      frame <- frameNew
                      tableAttachDefaults table frame 0 1 0 1
                      tableAttachDefaults table content 0 1 1 2
                      values <- entryNew
                      valuesb <- entryNew
                      salida <- entryNew
                      labelv <- labelNew (Just "Ingrese elementos de A o A aumentada por filas separadas por espacios")
                      labelb <- labelNew (Just "Ingrese elementos de la matriz b separados por espacios en el caso de los iterativos")
                      labels <- labelNew (Just "Resultado")
                      eval <- buttonNewWithLabel "Evaluar"
                         
                      boxPackStart content sistem  PackNatural 0
                      boxPackStart sistem  labelv  PackNatural 0
                      boxPackStart sistem  values  PackNatural 0
                      boxPackStart sistem  labelb  PackNatural 0
                      boxPackStart sistem  valuesb PackNatural 0
                      boxPackStart sistem  labels  PackNatural 0
                      boxPackStart sistem  salida  PackNatural 0
                      boxPackStart content eval  PackNatural 0

                  
                      boxPackStart content frame PackNatural 0
                      canvas <- drawingAreaNew
                      containerAdd frame canvas
                      widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

                      {-METODOS DIRECTOS-}

                      directos <- hBoxNew False 5
                      labeld <- labelNew (Just "Métodos directos")
                      containerAdd content labeld

                      {-ELIMINACION GAUSSIANA SIMPLE-}
                      gsim <- hBoxNew False 10
                      option <- vBoxNew False 0
                      containerAdd directos gsim
                      radio1 <- radioButtonNewWithLabel "Eliminación gaussiana simple"
                      boxPackStart option radio1 PackNatural 5
                      boxPackStart gsim option PackNatural 0
 
                      {-ELIMINACION CON PIVOTEO PARCIAL-}
                      gparcial <- hBoxNew False 10
                      optionp <- vBoxNew False 0
                      containerAdd directos gparcial
                      radio2 <- radioButtonNewWithLabelFromWidget radio1 "Eliminación con pivoteo parcial"
                      boxPackStart optionp radio2 PackNatural 5
                      boxPackStart gparcial optionp PackNatural 0
                      
                      {-ELIMINACION CON PIVOTEO TOTAL-}
                      gtotal <- hBoxNew False 25
                      optiont <- vBoxNew False 0
                      containerAdd directos gtotal
                      radio3 <- radioButtonNewWithLabelFromWidget radio2 "Eliminación con pivoteo total"
                      boxPackStart optiont radio3 PackNatural 5
                      boxPackStart gtotal optiont PackNatural 0
                      containerAdd content directos
                    
                      {-METODOS ITERATIVOS-}

                      iterativo <- vBoxNew False 5
                      labeliter <- labelNew (Just "Métodos iterativos")
                      containerAdd iterativo labeliter
                      tableAttachDefaults table iterativo 0 1 2 3
                      
                      {-JACOBI-}
                      jaco <- hBoxNew False 5
                      optionj <- vBoxNew False 5
                      bxoj <- vBoxNew  False 5
                      bnj <- vBoxNew  False 5
                      blaj <- vBoxNew  False 5
                      btolj <- vBoxNew  False 5
                      bij <- vBoxNew  False 5
                      containerAdd iterativo jaco
                      radio4 <- radioButtonNewWithLabelFromWidget radio3 "Jacobi"
                      boxPackStart optionj radio4 PackNatural 5
                      boxPackStart jaco  optionj PackNatural 0
                      boxPackStart jaco bxoj PackNatural 0
                      boxPackStart jaco bnj PackNatural 0
                      boxPackStart jaco blaj PackNatural 0
                      boxPackStart jaco btolj PackNatural 0
                      boxPackStart jaco bij PackNatural 0
                      
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
                      
                      {-GAUSS SEIDEL-}
                      seidel <- hBoxNew False 5
                      options <- vBoxNew False 5
                      bxos <- vBoxNew  False 5
                      bns <- vBoxNew  False 5
                      blas <- vBoxNew  False 5
                      btols <- vBoxNew  False 5
                      bis <- vBoxNew  False 5
                      containerAdd iterativo seidel
                      radio5 <- radioButtonNewWithLabelFromWidget radio4 "Gauss Seidel"
                      boxPackStart options radio5 PackNatural 5
                      boxPackStart seidel  options PackNatural 0
                      boxPackStart seidel bxos PackNatural 0
                      boxPackStart seidel bns PackNatural 0
                      boxPackStart seidel blas PackNatural 0
                      boxPackStart seidel btols PackNatural 0
                      boxPackStart seidel bis PackNatural 0
                      
                      labelas <- labelNew (Just "Valores iniciales")
                      miscSetAlignment labelas 0 0
                      boxPackStart bxos labelas PackNatural 0
                      xos <- entryNew
                      boxPackStart bxos xos PackNatural 0
                      
                      labelns <- labelNew (Just "Tamaño del sistema")
                      miscSetAlignment labelns 0 0
                      boxPackStart bns labelns PackNatural 0
                      ns <- entryNew
                      boxPackStart bns ns PackNatural 0

                      labellas <- labelNew (Just "Relajación")
                      miscSetAlignment labellas 0 0
                      boxPackStart blas labellas PackNatural 0
                      las <- entryNew
                      boxPackStart blas las PackNatural 0

                      labeltols <- labelNew (Just "Tolerancia")
                      miscSetAlignment labeltols 0 0
                      boxPackStart btols labeltols PackNatural 0
                      tols <- entryNew
                      boxPackStart btols tols PackNatural 0

                      labelis <- labelNew (Just "Numero maximo iteraciones")
                      miscSetAlignment labelis 0 0
                      boxPackStart bis labelis PackNatural 0
                      is <- entryNew
                      boxPackStart bis is PackNatural 0
                      onClicked eval $ do
                        set salida [entryText := "todavia no"]
                      return table
                      
                      
    


