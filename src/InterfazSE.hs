module InterfazSE where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign


sistemasEcuaciones :: IO Table
sistemasEcuaciones= do
                      table <- tableNew 2 1 False
                      content <- vBoxNew False 10
                      sistem <- vBoxNew False 10 
                      frame <- frameNew
                      tableAttachDefaults table content 0 1 0 1
                      values <- entryNew
                      labelv <- labelNew (Just "Ingrese elementos por filas separadas por comas")
                      boxPackStart content sistem PackNatural 5
                      boxPackStart sistem  labelv PackNatural 5
                      boxPackStart sistem  values PackNatural 5
                  
                      boxPackStart content frame PackNatural 5
                      canvas <- drawingAreaNew
                      containerAdd frame canvas
                      widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
                      return table
    


