module Testing where

import UU.Parsing
import Semantica
import GramaticaConcreta

parserTest1 :: IO ()
parserTest1 = do a <- parseIO pItg (itgScanTxt "|(0,1) x D x")
                 let b = "Cadena ingresada: '|(0,1) x D x'.\nResultado obtenido:\n"
                 putStr b
                 print a

parserTest2 :: IO ()
parserTest2 = do a <- parseIO pItg (itgScanTxt "|(0,1)|(1,2)|(2,3) x*y+z D x D y D z")
                 let b = "Cadena ingresada: '|(0,1)|(1,2)|(2,3) x*y+z D x D y D z'.\nResultado obtenido:\n"
                 putStr b
                 print a

parserTest3 :: IO ()
parserTest3 = do a <- parseIO pItg (itgScanTxt "|(0,1)|(2,5) y+x^2 D x D y")
                 let b =  "Cadena ingresada: '|(0,1)|(2,5) y+x^2 D x D y'.\nResultado obtenido:\n"
                 putStr b
                 print a

evalTest1 :: IO()
evalTest1 = do a <- parseIO pItg (itgScanTxt "|(0,1) x D x")
               let b = "Cadena ingresada: '|(0,1) x D x'.\nResultado obtenido:\n"
               let c = eval a
               putStr b
               print c

evalTest2 :: IO()
evalTest2 = do a <- parseIO pItg (itgScanTxt "|(0,1)|(1,2)|(2,3) x*y+z D x D y D z")
               let b = "Cadena ingresada: '|(0,1)|(1,2)|(2,3) x*y+z D x D y D z'.\nResultado obtenido:\n"
               let c = eval a
               putStr b
               print c

evalTest3 :: IO()
evalTest3 = do a <- parseIO pItg (itgScanTxt "|(0,1)|(2,5) y+x^2 D x D y")
               let b = "Cadena ingresada: '|(0,1)|(2,5) y+x^2 D x D y'.\nResultado obtenido:\n"
               let c = eval a
               putStr b
               print c

test = do putStr "Pruebas de los componentes del programa\nPrueba de los parser:\n"
          parserTest1 
          parserTest2
          parserTest3
          putStr "Prueba de la funcion de evaluacion:\n"
          evalTest1
          evalTest2
          evalTest3