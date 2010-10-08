module InterfazPrincipal where
import InterfazENL
import Graphics.UI.Gtk


main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "LambdaMethods", windowDefaultWidth := 800,
                 windowDefaultHeight := 800 ]
     
     ntbk <- notebookNew
     containerAdd window ntbk
     set ntbk [notebookScrollable := True, notebookEnablePopup := False,
                    notebookTabPos := PosLeft ]

     add_page ntbk ecuacionesNoLineales "Ecuaciones No Lineales"  
     onSwitchPage ntbk (putStrLn . ((++)"Page: ") . show)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI


add_page ::  Notebook -> IO Table-> String -> IO Int
add_page noteb c name  = do men <-  c
                            pagenum <- notebookAppendPage noteb men name
                            return pagenum
 