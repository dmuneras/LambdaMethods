module Main where

import Semantica
import GramaticaConcreta
import GramaticaAbstracta
import EcuacionesNoLineales
import UU.Parsing
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Prelude

process ::(EntryClass e)=> String -> e  -> IO ()
process s e = do a <- parseIO pFunc (funScanTxt s)
                 let st = show a --busqdIncremental a (FConst (0.0)) (FConst (1e-3)) 10
                 set e [ entryText := show st ]

main = do
  initGUI
  Just xml    <- xmlNew "interfazmethods.glade"
  ventana     <- xmlGetWidget xml castToWindow "window1"
  onDestroy ventana mainQuit
  boton   <- xmlGetWidget xml castToButton "button1"
  entrada <- xmlGetWidget xml castToEntry "entry1"
  salida  <- xmlGetWidget xml castToEntry "entry2"
  onClicked boton $ do
        texto <- get entrada entryText
        process texto salida
  onEntryActivate entrada $ do
        texto <- get entrada entryText
        process texto salida
  widgetShowAll ventana
  mainGUI
