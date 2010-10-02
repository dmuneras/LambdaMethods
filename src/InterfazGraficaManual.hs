module Main where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  w < - windowNew
  b < - buttonNew
  set w [ containerBorderWidth := 10,
               containerChild := button ]
  set b [ buttonLabel := "Hello World" ]
  onClicked b (putStrLn "Hello World")
  onDestroy w mainQuit
  widgetShowAll w
  mainGUI