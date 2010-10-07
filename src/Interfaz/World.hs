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
  ventana     <- windowNew
  onDestroy ventana mainQuit
  boton   <- buttonNew
  entrada <- entryNew
  salida  <- entryNew
  onClicked boton $ do
        texto <- get entrada entryText
        process texto salida
  onEntryActivate entrada $ do
        texto <- get entrada entryText
        process texto salida
  widgetShowAll ventana
  mainGUI
