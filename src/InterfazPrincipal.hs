module InterfazPrincipal where
import InterfazENL
import InterfazSE
import InterfazInterpolacion
import InterfazIntegracion
import Graphics.UI.Gtk


main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [windowTitle := "LambdaMethods", windowDefaultWidth := 400,
                 windowDefaultHeight := 800,  windowDecorated := True]
     
     box <- vBoxNew False 0 
     ntbk <- notebookNew 
     containerAdd window box
     set ntbk [notebookScrollable := True, notebookEnablePopup := False,
                    notebookTabPos := PosLeft ]


     
     fma <- actionNew "FMA" "File" Nothing Nothing
     ema <- actionNew "EMA" "Edit" Nothing Nothing
     hma <- actionNew "HMA" "Help" Nothing Nothing

     newa <- actionNew "NEWA" "New"     (Just "Just a Stub") (Just stockNew)
     exia <- actionNew "EXIA" "Exit"    (Just "Just a Stub") (Just stockQuit)

     cuta <- actionNew "CUTA" "Cut"   (Just "Just a Stub") (Just stockCut)    
     copa <- actionNew "COPA" "Copy"  (Just "Just a Stub") (Just stockCopy)
     psta <- actionNew "PSTA" "Paste" (Just "Just a Stub") (Just stockPaste)

     hlpa <- actionNew "HLPA" "Help"  (Just "Just a Stub") (Just stockHelp)

     agr <- actionGroupNew "AGR"
     mapM_ (actionGroupAddAction agr) [fma, ema, hma]
     mapM_ (\ act -> actionGroupAddActionWithAccel agr act Nothing) 
       [newa,cuta,copa,psta,hlpa]

     actionGroupAddActionWithAccel agr exia (Just "<Control>e")

     ui <- uiManagerNew
     uiManagerAddUiFromString ui uiDecl
     uiManagerInsertActionGroup ui agr 0

     maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
     let menubar = case maybeMenubar of
                        (Just x) -> x
                        Nothing -> error "Cannot get menubar from string." 
   

     maybeToolbar <- uiManagerGetWidget ui "/ui/toolbar"
     let toolbar = case maybeToolbar of
                        (Just x) -> x
                        Nothing -> error "Cannot get toolbar from string." 

     add_page ntbk ecuacionesNoLineales "Ecuaciones No Lineales"  
     add_page ntbk sistemasEcuaciones "Sistemas de Ecuaciones" 
     add_page ntbk interpolacion "Interpolación" 
     add_page ntbk interfaz_integracion "Integración"
     onSwitchPage ntbk (putStrLn . ((++)"Page: ") . show)
                  
    
     boxPackStart box menubar PackNatural 0
     boxPackStart box toolbar PackNatural 5
     boxPackStart box ntbk PackNatural 0

     actionSetSensitive cuta True
     onActionActivate exia (widgetDestroy window)

     widgetShowAll window
     onDestroy window mainQuit
     mainGUI


add_page ::  Notebook -> IO Table-> String -> IO Int
add_page noteb c name  = do men <-  c
                            pagenum <- notebookAppendPage noteb men name
                            return pagenum
    
uiDecl=  "<ui>\
\           <menubar>\
\            <menu action=\"FMA\">\
\              <menuitem action=\"NEWA\" />\
\              <separator />\
\              <menuitem action=\"EXIA\" />\
\            </menu>\
\           <menu action=\"EMA\">\
\              <menuitem action=\"CUTA\" />\
\              <menuitem action=\"COPA\" />\
\              <menuitem action=\"PSTA\" />\
\           </menu>\
\            <separator />\
\            <menu action=\"HMA\">\
\              <menuitem action=\"HLPA\" />\
\            </menu>\
\           </menubar>\
\           <toolbar>\
\            <toolitem action=\"NEWA\" />\
\            <toolitem action=\"EXIA\" />\
\            <separator />\
\            <toolitem action=\"CUTA\" />\
\            <toolitem action=\"COPA\" />\
\            <toolitem action=\"PSTA\" />\
\            <separator />\
\            <toolitem action=\"HLPA\" />\
\           </toolbar>\
\          </ui>"



 