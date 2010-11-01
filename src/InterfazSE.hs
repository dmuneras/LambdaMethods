module InterfazSE where

import FuncionesInterfaz
import GraficosFunciones
import GramaticaConcreta
import UU.Parsing
import Graphics.UI.Gtk
import Foreign


main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 30 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
     widgetShowAll window 
     onDestroy window mainQuit
     mainGUI


