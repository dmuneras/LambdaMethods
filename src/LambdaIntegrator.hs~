module Main where
import Semantica
import GramaticaConcreta
import GramaticaAbstracta
import UU.Parsing
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

process ::(EntryClass e)=> String -> e  -> IO ()
process s e = do a <- parseIO pItg (itgScanTxt s)
                 let st = eval a
                 set e [ entryText := show st ]

main = do
  initGUI
  Just xml    <- xmlNew "interfazintegrador.glade"
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
